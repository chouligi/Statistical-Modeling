library(xtable)
library(tikzDevice)

data = read.table("birthweight.txt",header=TRUE)
#convert int to factors
data$Race = factor(data$Race)
data$Smoker = factor(data$Smoker)
data$Hypertension = factor(data$Hypertension)
data$UterineIrrit = factor(data$UterineIrrit)

#variable low
data$low = data$BirthWeight < 2500
data$low = factor(data$low)

#informative plots

#to put plots aside
#par(mfrow=c(1,3)) 
plot(data$MotherAge, data$BirthWeight,
     main="Plot of Birth Weight vs Mother’s Age", xlab="Mother’s Age", ylab="Birth Weight")

plot(data$MotherWeight, data$BirthWeight,
     main="Plot of Birth Weight vs Mother’s Weight", xlab="Mother’s Weight", ylab="Birth Weight")

boxplot(data$BirthWeight~data$Smoker,
        main="Boxplot per mother's Smoking status", xlab="0=No, 1=Yes", ylab="Birth Weight")

boxplot(data$BirthWeight~data$Race,
        main="Boxplot per mother's Race", xlab="1 = white, 2 = black, 3 = other", ylab="Birth Weight")

boxplot(data$BirthWeight~data$PremLabor,
        main="Boxplot per mother's Premature Labor history", xlab="Number of Occurences", ylab="Birth Weight")

boxplot(data$BirthWeight~data$Hypertension,
        main="Boxplot per mother's Hypertension history", xlab="0=No, 1=Yes", ylab="Birth Weight")

boxplot(data$BirthWeight~data$UterineIrrit,
        main="Boxplot per mother's Uterine Irritability status ", xlab="0=No, 1=Yes", ylab="Birth Weight")

boxplot(data$BirthWeight~data$PhysicianVisits,
        main="Boxplot per mother's Physician Visits ", xlab="Number of Visits", ylab="Birth Weight")


#fit full model

full = glm(low ~ MotherAge + MotherWeight + Race + Smoker + PremLabor + Hypertension + UterineIrrit + PhysicianVisits, data = data, family = binomial(link='logit'))
summary(full)
AIC(full)
BIC(full)
anova(full, test = 'Chisq')


#fit reduced model

reduced = glm(low ~  MotherAge + MotherWeight + Smoker + PremLabor + Hypertension , data = data, family = binomial(link='logit'))
summary(reduced)
anova(reduced, test = "Chisq")
AIC(reduced)
BIC(reduced)


#estimates for the probability of low birth weight
prob = matrix(fitted(reduced))
prob

#setting threshold 0.5
pred = matrix(prob > 0.5)
pred = pred * 1


real = matrix(data$BirthWeight < 2500)
real = real * 1
table = cbind(prob,pred,real)

#accuracy 
acc = mean(real == pred)
acc
xtable(table)

#accuracy of the full model
prob2 = matrix(fitted(full))
pred2 = matrix(prob2 > 0.5)
pred2 = pred2 * 1

table2 = cbind(prob2,pred2,real)
acc2 = mean (real == pred2)
acc2

P = sum(residuals(reduced,"pearson")^2)
P
D = deviance(reduced)
D
df = reduced$df.residual


qchisq(0.95,df)

#residuals
resP = residuals(reduced,'pearson')
resD =residuals(reduced,'deviance')

#Deviance residuals against Pearson residuals
plot(resD,resP,xlab="Deviance Residuals",ylab="Pearson Residuals",
     main= "Deviance residuals against Pearson residuals", pch=20, cex=1, col="blue")
abline(0,1,lty=3)


