
####Objective: Produce at least one scatter plot, histogram, and box-and-whisker plot for each variable from the attitude data frame.
##Optional: To save time, explore creating a matrix of histograms, a matrix of scatter plots, and a matrix of boxplots.

#Scatter plot matrix using pairs function
pairs(attitude, main="Attitude of Clerical Employees")

#Or, plot a scatter plot matrix using the plot() function and adding a locally weighted scatterplot smoothing (lowess)
plot(attitude[,1:7], panel=panel.smooth, main="Attitude of Clerical Employees")

##Histograms
hist(attitude$rating,breaks=8,xlab="Rating", main="Overall Rating")
hist(attitude$complaints,breaks=8,xlab="Complaints", main="Handing of Employee Complaints")
hist(attitude$privileges,breaks=8,xlab="Privileges", main="Does Not Allow Special Privileges")
hist(attitude$learning,breaks=8,xlab="Learning", main="Opportunities to Learn")
hist(attitude$raises,breaks=8,xlab="Raises", main="Raises Based on Performance")
hist(attitude$critical,breaks=8,xlab="Critical", main="Too Critical")
hist(attitude$advance,breaks=8,xlab="Advance", main="Advancement")

##histogram matrix. The histograms are created using histogram function in the lattice package (slighty nicer)
##A trellis graphics window can also be split, but in this case the
##print method of the lattice package must be used on an object of class
##trellis (which might be built in the same command), using the split
## = optional argument to specify the position (x and y) within a matrix of
##plots. For all but the last plot on a page the more=T argument must be
#speciﬁed
require(lattice)
h1 =histogram(attitude$rating, breaks=8, xlab="Rating", main="Overall Rating")
h2 =histogram(attitude$complaints,xlab="Complaints", main="Handing of Employee Complaints")
h3 =histogram(attitude$privileges,xlab="Privileges", main="Does Not Allow Special Privileges")
h4 =histogram(attitude$learning,xlab="Learning", main="Opportunities to Learn")
h5 =histogram(attitude$raises,xlab="Raises", main="Raises Based on Performance")
h6 =histogram(attitude$critical,xlab="Critical", main="Too Critical")
h7 =histogram(attitude$advance,xlab="Advance", main="Advancement")

print(h1, split=c(1,1,4,4), more=T);
print(h2, split=c(2,1,4,4), more=T);
print(h3, split=c(3,1,4,4), more=T);
print(h4, split=c(4,1,4,4), more=T);
print(h5, split=c(1,2,4,4), more=T);
print(h6, split=c(2,2,4,4), more=T);
print(h7, split=c(3,2,4,4), more=F);
##remove the variables
rm(h1, h2, h3, h4, h5, h6, h7)

#Box and whisker plotted individually
boxplot(attitude$rating, main="Overall Rating")
boxplot(attitude$complaints, main="Handing of Employee Complaints")
boxplot(attitude$privileges, main="Does Not Allow Special Privileges")
boxplot(attitude$learning, main="Opportunities to Learn")
boxplot(attitude$raises, main="Raises Based on Performance")
boxplot(attitude$critical, main="Too Critical")
boxplot(attitude$advance, main="Advancement")

#Box and whisker plots matrix
boxplot(x = as.list(as.data.frame(attitude)), main="Attitude of Clerical Employees")
