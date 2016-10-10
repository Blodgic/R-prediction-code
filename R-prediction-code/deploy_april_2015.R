deploy.base = read.csv("/features_pre_process_iter3.csv")
str(deploy.base) 

# create and reclass training data
deploy.train = subset(deploy.base, select=c("CnBio.ID","CnAttrCat_5_01_Description", "CnAttrCat_4_01_Description", "CnMem.1.01.Standing", "CnMem_1_01_Cur_Category_description", "Prev_expiry_date", "Visits", "NetAmt", "Events", "Rental", "Bday_party", "Median", "Mean","Pop","Distance","Distance.lev"))
str(deploy.train)

deploy.train$Prev_expiry_date = as.Date(deploy.train$Prev_expiry_date, "%m/%d/%Y")
deploy.train = subset(deploy.train, Prev_expiry_date >= "2014-07-31" & Prev_expiry_date <= "2014-11-30")
deploy.train = subset(deploy.train, select=-c(Prev_expiry_date))

deploy.train$CnBio.ID = as.factor(deploy.train$CnBio.ID)
deploy.train$Distance.lev = as.factor(deploy.train$Distance.lev)
#####deploy.train$Median.lev = as.factor(deploy.train$Median.lev)

deploy.train$Visits = as.numeric(deploy.train$Visits)
deploy.train$Rental = as.numeric(deploy.train$Rental)
deploy.train$Bday_party = as.numeric(deploy.train$Bday_party)
deploy.train$Events = as.numeric(deploy.train$Events)

deploy.train$Median = as.numeric(deploy.train$Median)
deploy.train$Mean = as.numeric(deploy.train$Mean)
deploy.train$Pop = as.numeric(deploy.train$Pop)

deploy.train$CnMem.1.01.Standing = ifelse(deploy.train$CnMem.1.01.Standing == "Non Renewed", "NonRenewed", "renewed")
deploy.train$CnMem.1.01.Standing = as.factor(deploy.train$CnMem.1.01.Standing)

deploy.train = subset(deploy.train, select=-c(CnBio.ID))


# create and reclass test data
deploy.test.og = read.csv("/features_deploy_median_salary.csv")
str(deploy.test.og) 

deploy.test = deploy.test.og
deploy.test = subset(deploy.test, select=c("CnBio_ID","CnAttrCat_5_01_Description", "CnAttrCat_4_01_Description", "CnMem_1_01_Cur_Category_description", "Visits", "NetAmt", "Events", "Rental", "Bday_party", "Median", "Mean","Pop","Distance","Distance.lev"))

deploy.test$CnBio_ID = as.factor(deploy.test$CnBio_ID)
deploy.test$Distance.lev = as.factor(deploy.test$Distance.lev)
####deploy.test$Median.lev = as.factor(deploy.test$Median.lev)

deploy.test$Visits = as.numeric(deploy.test$Visits)
deploy.test$Rental = as.numeric(deploy.test$Rental)
deploy.test$Bday_party = as.numeric(deploy.test$Bday_party)
deploy.test$Events = as.numeric(deploy.test$Events)

deploy.test$Median = as.numeric(deploy.test$Median)
deploy.test$Mean = as.numeric(deploy.test$Mean)
deploy.test$Pop = as.numeric(deploy.test$Pop)

deploy.test = subset(deploy.test, select=-c(CnBio_ID))

library(caret)

# J48
library(RWeka)
set.seed(777)
dptrain = train(CnMem.1.01.Standing~.,
                data = deploy.train,
                method = "J48",
                trControl = trainControl(method='cv'))

dptrain

J48.predicted.prob = predict(dptrain, deploy.test, type = "prob")
J48.predicted.prob = as.data.frame(J48.predicted.prob)
head(J48.predicted.prob)

J48.predicted.class = predict(dptrain, deploy.test, type = "raw")
J48.predicted.class = as.data.frame(J48.predicted.class)
names(J48.predicted.class)[1] = "J48.predicted.class"

deploy.test.og$J48_Non_Renewed_prob = J48.predicted.prob$NonRenewed
deploy.test.og$J48_renewed_prob = J48.predicted.prob$renewed
deploy.test.og$J48_predicted_response = J48.predicted.class$J48.predicted.class

# randomForest
library(randomForest)
set.seed(777)
dptrain.rf = train(CnMem.1.01.Standing~.,
                   data = deploy.train,
                   method = "rf",
                   trControl = trainControl(method='cv'))

dptrain.rf

rf.predicted.prob = predict(dptrain.rf, deploy.test, type = "prob")
rf.predicted.prob = as.data.frame(rf.predicted.prob)
nrow(rf.predicted.prob)

rf.predicted.class = predict(dptrain.rf, deploy.test, type = "raw")
rf.predicted.class = as.data.frame(rf.predicted.class)
names(rf.predicted.class)[1] = "rf.predicted.class"

deploy.test.og$rf_Non_Renewed_prob = rf.predicted.prob$NonRenewed
deploy.test.og$rf_renewed_prob = rf.predicted.prob$renewed
deploy.test.og$rf_predicted_response = rf.predicted.class$rf.predicted.class

# glm
set.seed(777)
dptrain.glm = train(CnMem.1.01.Standing~.,
                    data = deploy.train,
                    method = "glm",
                    trControl = trainControl(method='cv'))

dptrain.glm

glm.predicted.prob = predict(dptrain.glm, deploy.test, type = "prob")
glm.predicted.prob = as.data.frame(glm.predicted.prob)
head(glm.predicted.prob)

glm.predicted.class = predict(dptrain.glm, deploy.test, type = "raw")
glm.predicted.class = as.data.frame(glm.predicted.class)
names(glm.predicted.class)[1] = "glm.predicted.class"

deploy.test.og$glm_Non_Renewed_prob = glm.predicted.prob$NonRenewed
deploy.test.og$glm_renewed_prob = glm.predicted.prob$renewed
deploy.test.og$glm_predicted_response = glm.predicted.class$glm.predicted.class

write.csv(deploy.test.og, "/leaf_deployment.csv" )
