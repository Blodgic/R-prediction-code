##Objective: a. Return back to part 5 on ordering factor variables. Just as you did with the access variable, create ordered factor variables for the excel, statistics and programming variables in the undergrad data frame. 
##b. Draw histograms for your new ordered factor variables you created in exercise a.

#renaming columns in the undergrad data frame
names(undergrad) = c("timestamp","excel","access", "statistics", "programming", "iscourse", "cscourse", "topics", "istopics", "onlinecourse", "concentration")

#attaching & viewing the undergrad data frame
attach(undergrad)
View(undergrad)

#creating a frequency table to determine histogram levels
table(excel)
#ordering the factor variables
excel_ordered=ordered(x=excel,levels=c("Somewhat agree", "Agree", "Strongly Agree"))
#drawing the histogram and casting the excel factor variable as a numeric. Required for the x value for the hist() function
hist(as.numeric(excel_ordered), breaks=7,main="Agreement: Importance of Learning Excel")

#creating a frequency table to determine histogram levels
table(statistics)
#ordering the factor variables
statistics_ordered=ordered(x=statistics,levels=c("Disagree", "Neither agree or disagree", "Somewhat agree", "Agree", "Strongly Agree"))
#drawing the histogram and casting the statistics factor variable as a numeric. Required for the x value for the hist() function
hist(as.numeric(statistics_ordered), breaks=7, main="Agreement: Importance of Learning Statistics")

#creating a frequency table to determine histogram levels
table(programming)
#ordering the factor variables
programming_ordered=ordered(x=programming,levels=c("Disagree", "Somewhat disagree", "Neither agree or disagree", "Somewhat agree", "Agree", "Strongly Agree"))
#drawing the histogram and casting the statistics factor variable as a numeric. Required for the x value for the hist() function
hist(as.numeric(programming_ordered, breaks=7), main="Agreement: Importance of Learning Programming")

