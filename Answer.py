



survey_file = open("survey_anon.txt", "r")

lines = 0
no_experience_count = 0
no_idea = 0
no_programming = 0

db_resp = set()
np_db_skills = {}
db_skills = {}

for line in survey_file:

    lines = lines + 1
    columns = line.strip().split("\t")

    if columns[1] == "I have no experience working in a terminal":
        no_experience_count = no_experience_count + 1
    elif line.count("I dont even understand the question") > 0:
        no_idea = no_idea + 1

    if line.count("I have never programmed before.") > 0:
        no_programming = no_programming + 1

        db_skill = columns[2]

# another way to do question 5
#        if db_skill in np_db_skills:
#            count = np_db_skills[db_skill]
#            np_db_skills[db_skill] = count + 1
#        else: 
#            np_db_skills[db_skill] = 1


        count = np_db_skills.get(db_skill, 0)
        np_db_skills[db_skill] = count + 1


    db_skill = columns[2]
    db_resp.add(db_skill)

    prog_skill = columns[3]
    db_skill_count = db_skills.get(prog_skill, {})
    
    count = db_skill_count.get(db_skill, 0)
    db_skill_count[db_skill] = count + 1

#    print "prog_skill is: %s " % prog_skill
#    print "db skill distribution is %s " % db_skill_count

    db_skills[prog_skill] = db_skill_count


print "there are %d lines in the file" % lines
print "there are %d students with no experience" % no_experience_count
print "there are %d students with no idea" % no_idea
print "there are %d students with no programming" % no_programming
print "these are the database skills %s" % db_resp

print "the no programming database skills are: %s" % np_db_skills
print "the overall db skill distribution is %s " % db_skills
