install.packages("coefplot")
require (coefplot)


boxoffice <- read.csv("~/Desktop/MSBA/code/movie.csv")



#RECODE GENRE VARIABLES OBSERVATIONS INTO THEIR CATEGORICAL NAMES
genrenames<-factor(boxoffice$Genre, 
                   levels=1:4, 
                   labels=c("action", "comedy", "kids", "other"))

#COMBINE THE CATEGORICAL NAME DATA FRAME WITH THE BOX OFFICE DATA FRAME
boxoffice2=data.frame(boxoffice, genrenames)


# check the distribution of key variables 
ggplot(boxoffice2, aes(x=Opening_Week_Revenue))+geom_histogram (binwidth=5)
ggplot(boxoffice2, aes(x=Opening_Week_Revenue, fill=genrenames))+geom_histogram (binwidth=5)

# check correlation b/w continous variables
cor(boxoffice2[,c(2:4)])

# Scatter Plot of Revenue and Theater
ggplot(boxoffice2, aes(x=Opening_Week_Revenue, y=Num_Theaters))+geom_point()+stat_smooth()



# DO A SCATTER PLOT of REVENUE & Rating
ggplot(boxoffice2, aes(x=Opening_Week_Revenue, y=Overall_Rating))+geom_point()+stat_smooth()

# Check Means by Genre 
aggregate(formula=Opening_Week_Revenue~genrenames,
          data=boxoffice2,
          FUN = mean)


#RUN THE REGRESSION (GENRE ONLY)
reg<-lm(Opening_Week_Revenue~genrenames, data=boxoffice2)
reg
coefplot(reg, intercept=TRUE)


#RUN LINEAR REGRESSION
Linregfull<-lm(Opening_Week_Revenue~genrenames+Num_Theaters+Overall_Rating, data=boxoffice2)

summary(Linregfull)
coefplot(Linregfull)

# Run a LOG BASED MODEL. 

#Take Log of Revenue, Theater, & Rating

boxoffice2$logRev<-log(boxoffice2$Opening_Week_Revenue)
boxoffice2$logTheater<-log(boxoffice2$Num_Theater)
boxoffice2$logRating<-log(boxoffice2$Overall_Rating)

# Run log regression

Logreg<-lm(logRev~genrenames+logTheater+logRating, data=boxoffice2)

summary(Logreg)
coefplot(Logreg)

