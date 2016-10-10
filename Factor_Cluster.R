#load necessary package

setwd("~/Desktop/myData/")
install.packages("psych")
library("psych")

store_demo <- read.csv("~/Desktop/myData/store_demo.csv")

#subset the demographics & shares
demo<-store_demo[, 16:25]
progshare<-store_demo[, 15]
share<-store_demo[,12:15]

#check correlations
cor(share)

z <- cor(demo)
z

#plot correlation matrix
require(lattice)
levelplot(z)

#run factor analysis
FA <- factanal(demo, 3, rotation="varimax", scores="regression")

#see various output in FA
names(FA)


#view factor analysis results
print(FA, cutoff=.5)

cor(FA$scores)

#save factor scores and bind scores to shares 
segment <- cbind(progshare, FA$scores)

##########CLUSTER ANALYSIS#######################

CLUSTER <- kmeans(segment, 4, nstart=20)

names(CLUSTER)
#see segment sizes
CLUSTER$size
#see segment interpretation
CLUSTER$centers

# append Factor Scores & cluster assignment to original data frame
store_final<- data.frame(store_demo, FA$scores, CLUSTER$cluster)

#Save the final File in working directory 
write.csv(store_final, file = "store_final.csv")
