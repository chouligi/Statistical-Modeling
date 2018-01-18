library(xtable)


chemo = read.table("chemo.txt",header=TRUE)


chemo$seizures = chemo$SeizuresP1 + chemo$SeizuresP2 + chemo$SeizuresP3 + chemo$SeizuresP4

#informative plots
plot(chemo$seizures, chemo$Treatment)
plot(chemo$Baseline, chemo$seizures, xlab ='Baseline Rate', ylab= 'Number of epileptic seizures', main ='Number of seizures vs Baseline Rate',pch =20)
plot(chemo$Age, chemo$seizures, xlab ='Age of patient (years)', ylab= 'Number of epileptic seizures', main ='Number of seizures vs Age of patient',pch =20)
boxplot(chemo$seizures ~chemo$Treatment, xlab = 'Treatment (0=placebo, 1=active)', ylab ='Number of epileptic seizures',main ='Number of seizures by Treatment',pch=20)
        
        




model1 = glm(seizures~Treatment + Baseline + Age, data = chemo, family = poisson)
summary(model1)
xtable(summary(model1))
beta = coefficients(model1)

#expected number of epilectic seizures for each patient
nseizures = matrix(fitted(model1))
nseizures
xtable(nseizures)
#95% confidence interval
FisherInv=vcov(model1)
df = model1$df.residual

#intercept
ub1 = beta[1] + qt(0.975,df)*sqrt(FisherInv[1,1])
lb1 = beta[1] - qt(0.975,df)*sqrt(FisherInv[1,1])
ci_intercept = c(lb1,ub1)

#treatment
ub2 = beta[2] + qt(0.975,df)*sqrt(FisherInv[2,2])
lb2 = beta[2] - qt(0.975,df)*sqrt(FisherInv[2,2])
ci_treatment = c(lb2,ub2)


#Baseline
ub3 = beta[3] + qt(0.975,df)*sqrt(FisherInv[3,3])
lb3 = beta[3] - qt(0.975,df)*sqrt(FisherInv[3,3])
ci_baseline = c(lb3,ub3)

#Age
ub4 = beta[4] + qt(0.975,df)*sqrt(FisherInv[4,4])
lb4 = beta[4] - qt(0.975,df)*sqrt(FisherInv[4,4])
ci_age = c(lb4,ub4)

confidence = rbind(ci_intercept, ci_treatment, ci_baseline, ci_age)
confidence
#Pearson chi-squared statistic
P = sum(residuals(model1,"pearson")^2)

#design matrix
X = model.matrix(model1)
#weight matrix
W = data.matrix(diag(model1$weights))

df = model1$df.residual
phi = P/df
If = phi*(t(X) %*% W %*% X)
If_inv = solve(If)
#square root of I diagonals
sq_I = sqrt(diag(If_inv))


#Plot of observed vs fitted values

plot(model1$fitted,chemo$seizures,xlab="Fitted Values",ylab="Observed Values", xlim = c(0,145), ylim = c(0,145),
     main= "Observed against Fitted values of the response", pch=20, cex=1, col="blue")
lines(c(0,145),c(0,145))

#Analysis of Deviance table with chi-square test
anova(model1,test = 'Chisq')
xtable(anova(model1,test = 'Chisq'))

#obtain deviance D
D = deviance(model1)

#residuals
resP = residuals(model1,'pearson')
resD =residuals(model1,'deviance')

#Deviance residuals against fitted values

plot(fitted(model1),resD,xlab="Fitted Values",ylab="Deviance Residuals",
     main= "Deviance residuals against Fitted values", pch=20, cex=1, col="blue")
#Pearson residuals against fitted values
plot(fitted(model1),resP,xlab="Fitted Values",ylab="Pearson Residuals",
     main= "Pearson residuals against Fitted values", pch=20, cex=1, col="blue")

#chi-square
qchisq(0.95,df)

#use of sqrt link function
model2 = glm(seizures~Treatment + Baseline + Age, data = chemo, family = poisson(link = 'sqrt'))
summary(model2)

P2 = sum(residuals(model2,"pearson")^2)
D2 = sum(residuals(model2,"deviance")^2)
P2
D2
anova(model2,test = 'Chisq')




