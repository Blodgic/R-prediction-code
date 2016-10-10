#Brennan Lodge
#April 26, 2014
#Exercise 5



attach(attitude)


layout.show(n = 1)

# Matrix of scatter plots for attitude

pairs(~rating+complaints+privileges+learning+raises+critical+advance, data=attitude, main="Attitude scatter plot matrix")

# Matrix of boxplots for attitude

histogram(~rating+complaints+privileges+learning+raises+critical+advance, data=attitude, main="Attitude histogram plot matix")

# Matrix of histogram for attitude

bwplot(~rating+complaints+privileges+learning+raises+critical+advance, data=attitude, main="Attitude boxplot matrix")


