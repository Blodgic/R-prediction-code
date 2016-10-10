#renaming columns in the undergrad data frame
names(undergrad) = c("timestamp","excel","access", "statistics", "programming", "iscourse", "cscourse", "topics", "istopics", "onlinecourse", "concentration")

access_ordered = ordered(x=access, levels=c("Strongly disagree","Disagree","Somewhat disagree","Neither agree or disagree","Somewhat agree","Agree","Strongly Agree"))