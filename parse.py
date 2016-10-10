%matplotlib inline
import matplotlib.pyplot as plt

survey_file = open("survey_anon.txt", "r")
lines = 0 

term_resp = set()
np_term_skills = {}

db_resp = set()
np_db_skills = {}

prog_resp = set()
np_prog_skills = {}


for line in survey_file:
	lines = lines + 1 

	columns = line.strip().split("\t")
	term_skill = columns[1]
	db_skill = columns[2]
	prog_skill = columns[3]

	count1 = np_term_skills.get(term_skill,0)
	np_term_skills[term_skill] = count1 + 1

	count2 = np_db_skills.get(db_skill, 0)
	np_db_skills[db_skill] = count2 + 1

	count3 = np_prog_skills.get(prog_skill, 0)
	np_prog_skills[prog_skill] = count3 + 1


	term_resp.add(term_skill)
	db_resp.add(db_skill)
	prog_resp.add(prog_skill)

	term_all_skills = dict.values(np_term_skills)
	db_all_skills = dict.values(np_db_skills)
	prog_all_skills = dict.values(np_prog_skills)
#print "these are the terminal skills %s \n" % term_resp
#print "these are the database skills %s \n" % db_resp
#print "these are the database skills %s \n" % prog_resp
print " the terinal skills are %s \n" % np_term_skills
print " the programming database skills are %s \n" % np_db_skills
print " the programming skills are %s \n" % np_prog_skills 
print "Terminal Skills" 
print term_all_skills
print "Database skills"
print db_all_skills
print "Programming skills"
print prog_all_skills

y = term_all_skills
x = db_all_skills
plt.plot(x, y)
