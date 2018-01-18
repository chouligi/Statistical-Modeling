### Exercise 1 ###

#lengths of datasets

n1 = 40  #I use small n's because of t-distribution
n2 = 45 

# the standard deviation (same for both datasets)

sd = sqrt(3)

# the two different means

mean1 = 30
mean2 = 32

# the 2 data sets generated from 2 normal distributions

set1 = rnorm(n1,mean1,sd)
set2 = rnorm(n2,mean2,sd)

# check the boxplot
boxplot(set1,set2, main="Boxplot of the two data sets", xlab = 'data sets')

# Perform t-test to check for equality of the population means
#H_0:means are the same, H_1: means are different

t.test(set1,set2, var.equal=TRUE, conf.level=0.95)

#p value is 4.216e-06 < 0.05 so the Null hypothesis is rejected
#and the true difference in means is significant

#Preparing the data for ANOVA 

#vector with all the observations

y=c(set1,set2)

#vector with factor containing 2 levels corresponding to the two data sets

effect = factor(c(rep(1,n1),rep(2,n2)))

#Data frame with y and effect

data = data.frame(y,effect)

#Create aov model

aovmodel=aov(y~effect,data=data)

#Obtain anova table

anova(aovmodel)

#summary(aovmodel)
#The p value 4.617e-06 is similar to the one obtained from t-test and again rejects the null
#hypothesis

#improve results by using the real variance in the calculation of T, instead of 
#the pooled variance

T=(mean1-mean2)/(sd*(sqrt(1/n1+1/n2)))

#calculate new p value
#You need the abs() function because otherwise you run the risk of getting p-values bigger than 1
#(when the mean of the data is bigger than the given mean)

p_value_real = 2*pt(-abs(T),df=n1+n2-2)

