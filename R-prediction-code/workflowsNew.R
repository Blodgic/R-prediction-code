## ===============================================
## The Box Plot Rule workflow
## ===============================================
BPrule.wf <- function(form,train,test,...) {
    ms <- as.matrix(filter(train,Insp != 'fraud') %>%
                         group_by(Prod) %>%
                         summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                         select(median,iqr))
    rownames(ms) <- levels(train$Prod)
    ms[which(ms[,'iqr']==0),'iqr'] <- ms[which(ms[,'iqr']==0),'median']
    ORscore <- abs(test$Uprice-ms[test$Prod,'median']) /
               ms[test$Prod,'iqr']
    rankOrder <- order(ORscore,decreasing=T)
    res <- WFoutput(evalOutlierRanking(test,rankOrder,...))
    workflowInformation(res) <-
        list(probs=matrix(c(ORscore,ifelse(test$Insp=='fraud',1,0)),ncol=2))
    res
}

## ===============================================
## The LOF workflow
## ===============================================
LOF.wf <- function(form, train, test, k, ...) {
   ntr <- nrow(train)
   all <- rbind(train,test)
   N <- nrow(all)
   ups <- split(all$Uprice,all$Prod)
   r <- list(length=ups)
   for(u in seq(along=ups)) 
     r[[u]] <- if (NROW(ups[[u]]) > 3) 
       lofactor(ups[[u]],min(k,NROW(ups[[u]]) %/% 2)) 
   else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]])) 
   else NULL
   all$lof <- vector(length=N)
   split(all$lof,all$Prod) <- r
   all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))] <- 
     SoftMax(all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))])

   res <- WFoutput(evalOutlierRanking(test,
              order(all[(ntr+1):N,'lof'],decreasing=T),...))
   workflowInformation(res) <-
       list(probs=matrix(c(all[(ntr+1):N,'lof'],
                ifelse(test$Insp=='fraud',1,0)),ncol=2))
   res
 }


## ===============================================
## The NB with Smote workflow
## ===============================================
NBsm.wf <- function(form,train,test,...) {
    require(e1071,quietly=TRUE)
    require(DMwR,quietly=TRUE)
    sup <- which(train$Insp != 'unkn')
    data <- train[sup,c('ID','Prod','Uprice','Insp')]
    data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
    newData <- SMOTE(Insp ~ .,data,perc.over=700)
    model <- naiveBayes(Insp ~ .,newData)
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],type='raw')
    rankOrder <- order(preds[,'fraud'],decreasing=T)
    rankScore <- preds[,'fraud']
    
    res <- WFoutput(evalOutlierRanking(test,rankOrder,...))

    workflowInformation(res) <-
        list(probs=matrix(c(rankScore,ifelse(test$Insp=='fraud',1,0)),ncol=2))
    res
}



## ===============================================
## The AdaBoosting workflow
## ===============================================
ab.wf.x <- function(form,train,test,ntrees=100,...) {
    require(adabag,quietly=TRUE)
    require(rpart,quietly=TRUE)
    sup <- which(train$Insp != 'unkn')
    data <- train[sup,c('ID','Prod','Uprice','Insp')]
    data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
    model <- boosting(Insp ~ .,data,mfinal=ntrees)
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')])
    rankOrder <- order(preds$prob[,2],decreasing=TRUE)
    rankScore <- preds$prob[,2]
    
    res <- WFoutput(evalOutlierRanking(test,rankOrder,...))
    workflowInformation(res) <-
        list(probs=matrix(c(rankScore,ifelse(test$Insp=='fraud',1,0)),ncol=2))
    res
}

# Weka version
ab.wf <- function(form,train,test,ntrees=100,...) {
    require(RWeka,quietly=TRUE)
    sup <- which(train$Insp != 'unkn')
    data <- train[sup,c('ID','Prod','Uprice','Insp')]
    data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
    model <- AdaBoostM1(Insp ~ .,data,
                       control=Weka_control(I=ntrees))
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                     type='probability')
    rankOrder <- order(preds[,"fraud"],decreasing=TRUE)
    rankScore <- preds[,"fraud"]
    
    res <- WFoutput(evalOutlierRanking(test,rankOrder,...))
    workflowInformation(res) <-
        list(probs=matrix(c(rankScore,ifelse(test$Insp=='fraud',1,0)),ncol=2))
    res
}



ab.st.wf <- function(train,test,ntrees=100,...) {
    require(RWeka,quietly=TRUE)
    require(DMwR,quietly=TRUE)
    train <- train[,c('ID','Prod','Uprice','Insp')]
    train[which(train$Insp == 'unkn'),'Insp'] <- NA
    train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
    pred.ada <- function(m,d) {
        p <- predict(m,d,type='probability')
        data.frame(cl=colnames(p)[apply(p,1,which.max)],
                   p=apply(p,1,max)
                   )
    }

    model <- SelfTrain(Insp ~ .,train,
                       learner('AdaBoostM1',
                               list(control=Weka_control(I=ntrees))),
                       'pred.ada')
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                     type='probability')

    rankOrder <- order(preds[,'fraud'],decreasing=T)
    rankScore <- preds[,"fraud"]
    
    res <- WFoutput(evalOutlierRanking(test,rankOrder,...))
    workflowInformation(res) <-
        list(probs=matrix(c(rankScore,ifelse(test$Insp=='fraud',1,0)),ncol=2))
    res
    
}
