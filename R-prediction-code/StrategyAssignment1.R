
#stragetgy assignment 1
library(coefplot)
library(ggplot2)
library(Hmisc)

HDLo <- read.csv(file.choose(), sep = ",", fill = TRUE)

#clean up the dataframe of na's
HDLo.clean <- na.omit(HDLo) 

#understanding the dataset
summary(HDLo.clean)
str(HDLo.clean)
names(HDLo.clean)
describe(HDLo.clean)
dim(HDLo.clean)

#Dummy Variables to identify if HomeDepot or Lowes are present in the county
HDLo.clean$HD_present <- ifelse(HDLo.clean$HDcount > 0, 1,0)
HDLo.clean$Lo_present <- ifelse(HDLo.clean$Lcount > 0, 1,0)

#regression model to identify the strength in the coefficents
reg1 <- lm(HDLo.clean$HD_present ~ HDLo.clean$Lo_present + pop_2000 +     
           pop_2010 + income_2000 + income_2010 + pct_U18_2000 +   
           pct_U18_2010 + pctcollege_2000 + pctcollege_2010 + ownhome_2000 +   
           ownhome_2010 + density_2000 + density_2010 + pctwhite_2000 +
           pctwhite_2010 + pctblack_2000 + pctblack_2010, data=HDLo.clean)
reg_summary <- summary(reg1)
reg_summary
coefplot(reg1)

#Correlation Matrix
x <- HDLo.clean[24:25]
y <- HDLo.clean[8:23]
correlation <- cor(x,y) 
write.csv(correlation, "Desktop/correlation.csv")

#HD prediction using glm binomial 
HD.glm <- glm(HDLo.clean$HD_present ~ 
                log(pop_2010 + 1) + income_2010 +  ownhome_2010 +
                ownhome_2010 +  log(density_2010 + 1) + pctcollege_2010 +
                pctwhite_2010  + pctblack_2010, family=binomial(link=logit), 
              data=HDLo.clean)
summary(HD.glm)
confint(HD.glm)
HDLo.clean$HD_predict <- predict(HD.glm, HDLo.clean)

HDLo.clean <- HDLo.clean[with(HDLo.clean, order(-HD_predict)),]
HDLo.clean.predict <- HDLo.clean[ which(HDLo.clean$HD_present == 0),]

top_5_HD_counties <- head(HDLo.clean.predict)
View(top_5_HD_counties)
write.csv(top_5_HD_counties, "Desktop/top_5_HD_counties.csv")

#Lowes Prediction using glm binomial
LO.glm <- glm(HDLo.clean$Lo_present ~ 
                          log(pop_2010 + 1) + income_2010 +  ownhome_2010 +
                          ownhome_2010 +  log(density_2010 + 1) + pctcollege_2010 +
                          pctwhite_2010  + pctblack_2010, family=binomial(link=logit), 
                        data=HDLo.clean)

HDLo.clean$LO_predict <- predict(LO.glm, HDLo.clean)

HDLo.clean <- HDLo.clean[with(HDLo.clean, order(-LO_predict)),]

HDLo.clean.predict_LO <- HDLo.clean[ which(HDLo.clean$Lo_present == 0),]
top_5_LO_counties <- head(HDLo.clean.predict_LO)
write.csv(top_5_LO_counties, "Desktop/top_5_LO_counties.csv")
View(top_5_LO_counties)
