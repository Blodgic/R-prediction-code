
##Objective: Create an R script that computes the measures of central tendency and measures of variability and the relationships for each of the seven variables in the attitude dataset. Use the functions below:
##mean, median, mode, max, min, range, quantile, IQR, var(), st(), and cor() 
##Check your work by using the summary and/or describe functions.

#View the attitude data frame
View(attitude)

#Compute the mean for each of the 7 variables
mean(attitude$rating)
mean(attitude$complaints)
mean(attitude$privileges)
mean(attitude$learning)
mean(attitude$raises)
mean(attitude$critical) 
mean(attitude$advance)

#Compute the median for each of the 7 variables
median(attitude$rating)
median(attitude$complaints)
median(attitude$privileges)
median(attitude$learning)
median(attitude$raises)
median(attitude$critical) 
median(attitude$advance)

#computing the mode for each variable using the algorithm from the lesson
#the mode is really only useful when one value has a higher frequency than most of the the other values
#the privileges variable has a single mode, whereas 

#rating mode
modeforrating<-table(as.vector(attitude$rating))
names(modeforrating)[modeforrating==max(modeforrating)]

#complaints mode
modeforcomplaints<-table(as.vector(attitude$complaints))
names(modeforcomplaints)[modeforcomplaints==max(modeforcomplaints)]

#privileges mode
modeforprivileges<-table(as.vector(attitude$privileges))
names(modeforprivileges)[modeforprivileges==max(modeforprivileges)]

#learning mode
modeforlearning<-table(as.vector(attitude$learning))
names(modeforlearning)[modeforlearning==max(modeforlearning)]

#raises mode
modeforraises<-table(as.vector(attitude$raises))
names(modeforraises)[modeforraises==max(modeforraises)]

#critical mode
modeforcritical<-table(as.vector(attitude$critical))
names(modeforcritical)[modeforcritical==max(modeforcritical)]

#advance mode
modeforadvance<-table(as.vector(attitude$advance))
names(modeforadvance)[modeforadvance==max(modeforadvance)]

#computing the max value for each variable
max(attitude$rating)
max(attitude$complaints)
max(attitude$privileges)
max(attitude$learning)
max(attitude$raises)
max(attitude$critical) 
max(attitude$advance)

#computing the min value for each variable
min(attitude$rating)
min(attitude$complaints)
min(attitude$privileges)
min(attitude$learning)
min(attitude$raises)
min(attitude$critical) 
min(attitude$advance)

#computing the range for each variable
range(attitude$rating)
range(attitude$complaints)
range(attitude$privileges)
range(attitude$learning)
range(attitude$raises)
range(attitude$critical) 
range(attitude$advance)

#computing the quantiles for each variable
quantile(attitude$rating)
quantile(attitude$complaints)
quantile(attitude$privileges)
quantile(attitude$learning)
quantile(attitude$raises)
quantile(attitude$critical) 
quantile(attitude$advance)

#computing the interquartile range (IQR) for each variable
IQR(attitude$rating)
IQR(attitude$complaints)
IQR(attitude$privileges)
IQR(attitude$learning)
IQR(attitude$raises)
IQR(attitude$critical) 
IQR(attitude$advance)

#computing the variance for each variable
var(attitude$rating)
var(attitude$complaints)
var(attitude$privileges)
var(attitude$learning)
var(attitude$raises)
var(attitude$critical) 
var(attitude$advance)

#computing the standard deviation for each variable
sd(attitude$rating)
sd(attitude$complaints)
sd(attitude$privileges)
sd(attitude$learning)
sd(attitude$raises)
sd(attitude$critical) 
sd(attitude$advance)

#computing the correlations between the 7 variables
cor(attitude)

#checking your work using the describe function from the psych package or summary
require(psych)
describe(attitude)
summary(attitude)
