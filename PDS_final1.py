import matplotlib.pyplot as plt
import numpy as np



survey_file = open("survey_anon.txt", "r")

lines = 0 
terminal_experiance = 0 
database_experiance = 0 
programming_experiance = 0 
no_experiance_count = 0 
no_idea = 0
no_programming = 0 

for line in survey_file:
	lines = lines + 1
  

	if line.count("NA") > 0: 
		continue

	
	if line.count ("I have written simple terminal commands or done some system work on the terminal") > 0:
		
		terminal_experiance = terminal_experiance + 1
		
	if line.count ("I can write very complex queries when needed") > 0:
		
		database_experiance = database_experiance + 1


	if line.count ("I can write complex programs, am familiar with programming design patterns, software testing, system design, and algorithms.") > 0:
		programming_experiance = programming_experiance + 1

	if line.count ("I have no experience working in a terminal") > 0:
		no_experiance_count = no_experiance_count + 1

	if line.count ("I dont even understand the question") > 0:
		no_idea = no_idea + 1 
	
	if line.count ("I have never programmed before.") > 0:
		no_programming = no_programming + 1 

print "there are %r students with a lot of terminal experiance" % terminal_experiance
print "there are %r students with a lot of database eperiance" % database_experiance
print "there are %r students with a lot of programming experiance" % programming_experiance
print "there are %r students with no experiance working in terminal" % no_experiance_count
print "there are %r students with no idea what a database is" % no_idea
print "there are %r students with no programming experiance" % no_programming


vals = [15,10,7,11,14,8]
xval = [0,1,2,3,4,5]
plt.bar(xval, vals)
plt.show()
