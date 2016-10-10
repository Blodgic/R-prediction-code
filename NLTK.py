import 
for line in open("JP_NA1A.csv"):
	for word in line.split():
		if word.endswith('ing'):
			print word