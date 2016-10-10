import matplotlib.pyplot as plt 

file = open("surve_anon.txt", "r")

line_count = 0 
terminal_experiance = 0 
database_experiance = 0 
programming_experiance = 0 
no_experiance_count = 0 
no_idea = 0
no_programming = 0 

for line in file:
	if line.count("NA"):
		continue
	line_count = line_count + 1

	if line.count ("I have written simple terminal commands or done some system work on the terminal") > 0:
		terminal_experiance = terminal_experiance + 1
	elif line.count ("I can write very complex queries when needed") > 0:
		database_experiance = database_experiance + 1
	elif line.count ("I can write complex program, am familar with programming design patterns, software testing, system design, and algorithms.") > 0:
		programming_experiance = programming_experiance + 1
	elif line.count("I have no experiance working in a terminal") > 0:
		no_experiance_count = no_experiance_count + 1
	elif line.count("I dont even understand the question") > 0:
		no_idea = no_idea + 1 
	elif line.count("I have never programmed before.") > 0:
		no_programming = no_programming + 1 

print "there are %d students with a lot of terminal experiance" % terminal_experiance
print "there are %d students with a lot of database eperiance" % database_experiance
print "there are %d students with a lot of programming experiance" % programming_experiance
print "there are %d students with no experiance working in terminal" % no_experiance_count
print "there are %d students with no idea what a database is" % no_idea
print "there are %d students with no programming experiance" % no_programming

 