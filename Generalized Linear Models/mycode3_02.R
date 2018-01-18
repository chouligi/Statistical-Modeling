library(ggplot2)
library(xtable)


clotting<-data.frame(conc=c(5,10,15,20,30,40,60,80,100,5,10,15,20,30,40,60,80,100),lot=factor(c(rep(1,9),rep(2,9))),time=c(118,58,42,35,27,25,21,19,18,69,35,26,21,18,16,13,12,12))


#informative plots
plot(clotting$time, log(clotting$conc))
plot(clotting$time, clotting$lot)

boxplot(clotting$time ~ clotting$lot, xlab = 'Lot of thromboplastin', ylab ='Clotting time (seconds)', main = 'Clotting time by lot of thromboplastin')

model1 = glm(time~log(conc) + lot, data = clotting, family = Gamma(link= 'inverse'))
summary(model1)
beta = coefficients(model1)
#plot of clotting against log(conc)
ggplot(clotting, aes(log(conc),time,shape=lot))+ 
  geom_point()+
  ggtitle("Clotting time against log(conc)")+
  labs(x="logarithm of concentration",y="Clotting time (seconds)") 

#expected clotting time for each individual
times = matrix(fitted(model1))


#90% confidence interval
FisherInv=vcov(model1)
df = model1$df.residual

#intercept
ub1 = beta[1] + qt(0.95,df)*sqrt(FisherInv[1,1])
lb1 = beta[1] - qt(0.95,df)*sqrt(FisherInv[1,1])
ci_intercept= c(lb1,ub1)


#log(conc)
ub2 = beta[2] + qt(0.95,df)*sqrt(FisherInv[2,2])
lb2 = beta[2] - qt(0.95,df)*sqrt(FisherInv[2,2])
ci_log_conc = c(lb2,ub2)


#lot
ub3 = beta[3] + qt(0.95,df)*sqrt(FisherInv[3,3])
lb3 = beta[3] - qt(0.95,df)*sqrt(FisherInv[3,3])
ci_lot = c(lb3,ub3)

confidence = rbind(ci_intercept,ci_log_conc,ci_lot)
confidence

#Pearson chi-squared statistic
P = sum(residuals(model1,"pearson")^2)
df = model1$df.residual
phi = P/df

phi
#Plot of observed vs fitted values

plot(model1$fitted[1:9], clotting$time[1:9],ylim = c(12,145), xlim = c(12,145), xlab = "Fitted Values", ylab = "Observed Values",  main= "Observed against Fitted values of clotting time", pch=20, cex=1, col="blue" )
points(model1$fitted[10:18], clotting$time[10:18],ylim = c(12,145), xlim = c(12,145), pch =20, col ='red')
abline(0,1,lty=3)
legend("bottomright", legend = c("lot 1","lot2"), pch=c(20,20),lwd=1,lty=c(NA,NA), col=c("blue","red"),bty = 'n')


#Analysis of deviance table with F test

anova(model1, test ="F")

#manual calculation of p-value using t-quantiles
t_log=coef(model1)[2]/sqrt(FisherInv[2,2]) 

2*(1-pt(abs(t_log),df))

t_lot=coef(model1)[3]/sqrt(FisherInv[3,3]) 
2*(1-pt(abs(t_lot),df))




#obtain deviance D
D = deviance(model1)

P/phi
D/phi
#residuals
resP = residuals(model1,'pearson')
resD =residuals(model1,'deviance')

#Deviance residuals against Pearson residuals
plot(resD,resP,xlab="Deviance Residuals",ylab="Pearson Residuals",
     main= "Deviance residuals against Pearson residuals", pch=20, cex=1, col="blue")
abline(0,1,lty=3)

qchisq(0.95,df)

