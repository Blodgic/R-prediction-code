#create an array of data mining grades
dataminingGrades=c(90,87,69,89,58,93,99,98,76,88)

#sort by decending
sort(DataMiningGrades, decreasing = FALSE) #(lowest to highest)

#get the class of the data type
class(DataMiningGrades)

#change an item in the array
dm[3]="B-"

#pass lettergradefactor as a function of LetterGrade
lettergradefactor = factor(LetterGrade)
lettergradefactor(lettergradefactor, levels=c("A+","A","A-","B+","B-","C+","C","C-","D+","D","D-","F"))

#create the variable
agreement =c("Disagree","Neigther agree or disagree", "Somewhat agree", "Agree", "Strongly Agree")

#view the variable
agreement

#convert to factor

counter = 0 
while (counter<9) {
  print(counter)
  counter = counter + 1 
  }

for (value in 1:10)
{
  print(value)
}

for (i in internet$country)
{
  caps = toupper(i)
  print (caps)
}



