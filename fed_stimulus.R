#Brennan Lodge
#April 23, 2014

# the sum for the payment value column and remove NAs
print(sum(fed_stimulus$Payment.Value, na.rm=TRUE))

# the mean for the payment value columnand remove NAs
print(mean(fed_stimulus$Payment.Value, na.rm=TRUE))

# project status is equal to the completed 50% or more
fed_stimulus_subset= subset(x= fed_stimulus,subset = Project.Status == "Completed 50% or more",select = Project.Name)
print(fed_stimulus_subset)

