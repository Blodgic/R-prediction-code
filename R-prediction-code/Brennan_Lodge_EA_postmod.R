###Brennan Lodge###
###Energy Analytics ##
##Assignment 5: Household Analysis in R ###
library(caret)
library(data.table)


#read file from an input selection
household <- read.csv(file.choose(), sep = ",", fill = TRUE)

###PREPROCESSING###

#remove quotes in Tariff Column
household$Tariff <- as.factor(gsub("'", "", household$Tariff))

#strip out "Tarif" from Tariff Column and turn it into a numeric field
household$Tariff <- as.factor(gsub("Tariff ", "", household$Tariff))

#remove target variable and get predicive variables
predic = names(household)[-12]
# numeric columns
num = predic[c(1,3,4)]
# categorical columns
cgr = predic[c(2,5,6,9,11)]
# binary columns
bin = predic[c(7,8,10)]

#convert cgr and bin to factors using the caret package
for (col in c(cgr, bin)) {
  set(household, j = col, value = as.factor(household[[col]]))
}

#split data into training and test data leveraging the caret package
#http://topepo.github.io/caret/splitting.html
set.seed(3456)
train.Index <- createDataPartition(household$Tariff, p = .66)[[1]]

train.Set <- household[ train.Index, ]
test.Set <- household[-train.Index,]


test.Results = data.frame("Tariff" = test.Set$Tariff)
test.Results
#decsion tree
require(rpart)
set.seed(3456)
rpart.Fit <- rpart(Tariff ~ ., data = train.Set, method = "class")

#Variable importance
rpart.Fit$variable.importance


printcp(rpart.Fit)

require(partykit)
plot(as.party(rpart.Fit))

plot(rpart.Fit)
text(rpart.Fit, pretty = 0)

test.Results$rpart_pred = predict(rpart.Fit, test.Set, type = "class")
rpart.matrix <- confusionMatrix(test.Results$rpart_pred, test.Results$Tariff)
rpart.matrix

#randomforest
library(randomForest)
set.seed(3456)
fit.rf <- randomForest(Tariff ~ ., data=train.Set, ntree= 500)
fit.rf
test.Results$rf_predict <- predict(fit.rf, test.Set, method = "class")
plot(fit.rf, main="Random Forest", log="y")
legend("right", colnames(fit.rf$err.rate), col=1:4,cex=0.66,fill=1:4)

fit.rf$confusion
fit.rf$ntree


rf.matrix <- confusionMatrix(test.Results$rf_predict, test.Results$Tariff)
plot(fit.rf)

#Cross Validation
#10 fold
cv.ctrl.10 <- trainControl(method = "cv", number = 10, summaryFunction= defaultSummary,
                        classProbs= TRUE, savePredictions = TRUE)




#Cross Validation Decsion Tree
set.seed(3456)
rpart.ft.cv = train(x = train.Set[ , predic],
                   y = train.Set$Tariff,
                   method = "rpart",
                   tuneLength = 30,
                   metric = "Accuracy",
                   trControl = cv.ctrl.10)


#Cross Validation Results for Decsion Tree
rpart.ft.cv
#10 Fold
rpart.ft.cv$resample
# Standard Deviation 3%
sd(rpart.ft.cv$resample$Accuracy) 
# Mean 79.5%
mean(rpart.ft.cv$resample$Accuracy) 
test.Results$rpart.cv.pred <- predict(rpart.ft.cv, test.Set)
## confusion matrix

rf.cv.cm = confusionMatrix(test.Results$rpart.cv.pred, test.Results$Tariff)
rf.cv.cm

plot(rpart.ft.cv$finalModel)
text(rpart.ft.cv$finalModel, pretty = 0)
plot(as.party(rpart.ft.cv$finalModel))

#Cross Validation for Random Forest
set.seed(2345)
cv.Fit.rf = train(x = train.Set[, predic],
                y = train.Set$Tariff,
                method = "rf",
                ntrees = 500,
                tuneGrid = data.frame(mtry = c(1:11)),
                metric = "Accuracy",
                trControl = cv.ctrl.10)

#Cross Validation Results for Random Forest
#10 fold 
cv.Fit.rf$resample 
mean(cv.Fit.rf$resample$Accuracy) #mean of accuracy is 81%
sd(cv.Fit.rf$resample$Accuracy) #sd of 4%
test.Results$cv.Fit.rf.pred <- predict(cv.Fit.rf, test.Set)

cf.Fit.rf <- confusionMatrix(test.Results$cv.Fit.rf.pred, test.Results$Tariff)
cf.Fit.rf

plot(cv.Fit.rf$finalModel)

