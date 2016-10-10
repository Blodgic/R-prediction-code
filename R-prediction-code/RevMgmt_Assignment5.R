
#NonStudent_WTP_data
NonStudent <- read.csv(file.choose(), sep = ",", header=TRUE, check.names=FALSE)

NonStudent <- as.data.table(NonStudent)
library(data.table)
NonStudent.dt <- data.table(NonStudent)
names(NonStudent.dt)
NonStudent.dt[, c('V8', 'V9','V10','V11','V12','V13','V14','V15') := NULL]
View(NonStudent.dt)
names(NonStudent.dt)
row.names(NonStudent.dt)
NonStudent.dt <- NonStudent.dt[-c(81:95)]

NonStudent.df <- as.data.frame(NonStudent.dt)
clean <- function(ttt){
  as.numeric( gsub('[^a-zA-Z0-9.]', '', ttt))
}
NonStudent.df[] <- sapply(NonStudent.df, clean)


summary(NonStudent.df)

library(Hmisc)
describe(NonStudent.df)
library(pastecs)
stat.desc(NonStudent.df) 

#Student 
Student <- read.csv(file.choose(), sep = ",", header=TRUE, check.names=FALSE)
View(Student)
Student[] <- sapply(Student, clean)
View(Student)
describe(Student)
stat.desc(Student)

#All survey Data
Survey <- read.csv(file.choose(), sep = ",", header=TRUE, check.names=FALSE)
View(Survey)
Survey[] <- sapply(Survey, clean)
describe(Survey)
stat.desc(Survey)
