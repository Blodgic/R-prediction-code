import	urllib2
import sklearn.tree as tree

url = "https://raw.githubusercontent.com/jattenberg/PDS-Spring-2014/master/data/marketing.data"

response = urllib2.urlopen(url)

# if is local do:
# file = open("path to your file", "r")

targets = [ ]
features = [ ] 


for line in response: 
	if line.count("NA") > 0 or len(line) < 5: 
		continue

		parts = line.strip().split(" ")
		numbers = [ ]
		for part in parts:
			numbers.append(int(part))

targets.append(numbers[0])
features.append(numbers[1:])

model1 = tree.DecisionTreeClassifier()
model1.fit(features, targets)

print model1
print model2