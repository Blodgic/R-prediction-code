import urllib2
import sklearn.tree as tree
import sklearn.cross_validation as cv
import numpy as np
import sklearn.svm as svm

url = "https://raw.githubusercontent.com/jattenberg/PDS-Spring-2014/master/data/marketing.data"

response = urllib2.urlopen(url)


# if is local do:
# file = open("path to your file", "r")

targets = [ ]
features = [ ]

test_targets = [ ]
test_features = [ ]

counter = 0

for line in response:
    # filter any lines with NAs or without any content
    if line.count("NA") > 0 or len(line) < 5:
        continue

    parts = line.strip().split(" ")

    # make sure that there are enough parts to process
    if len(parts) == 0:
        continue


    numbers = [ ]

    for part in parts:
        numbers.append(int(part))

    # partition the data
    if counter % 2 == 0:
        targets.append(numbers[0])
        features.append(numbers[1:])
    else:
        test_targets.append(numbers[0])
        test_features.append(numbers[1:])

    counter = counter + 1


model1 = tree.DecisionTreeClassifier()
model1.fit(features, targets)


model2 = tree.DecisionTreeRegressor()
model2.fit(features, targets)

model3 = svm.SVC()
model3.fit(features, targets)

print model1 
print model2

print model1.predict(test_features)
print model2.predict(test_features)

print model1.score(test_features, test_targets)
print model2.score(test_features, test_targets)

scores = cv.cross_val_score(model3, features, np.array(targets), cv=20, scoring='accuracy')

print scores
print scores.mean()

