weeds = read.table("Weeds.txt",header=TRUE)



#scatterplot of data

plot(weeds$Rate,weeds$Number,xlab="Rate of weed (kg/ha)",ylab="Number of healthy crops", main = 'Scatterplot of the data') 

form1=as.formula(Number~d + (a - d)/(1 + exp(b*log(g*Rate))))
form2=as.formula(Number~g + a*exp(-b*Rate))




f1=function(r,a,b,g,d)return(d + (a - d)/(1 + exp(b*log(g*r))))
f2=function(r,a,b,g)return(g + a*exp(-b*r))

#fit model using f1

model1 = nls(form1,data = weeds, start=c(a=20,b=4,g=3,d=5))
summary(model1)
coef1 = coef(model1)
coef1

plot(weeds$Rate,weeds$Number,xlab="Rate of weed (kg/ha)",ylab="Number of healthy crops", main = 'Scatterplot with fitted curves') 
legend('topright', legend = c('Model using f1','Model using f2'), lty=c(1,1),lwd=c(1,1),col=c('blue','red'),bty = 'n')

#using lines
x=seq(from=0.0,to=0.72,by=0.01)
lines(x,f1(x,coef1[1],coef1[2],coef1[3],coef1[4]),col="blue")
#using curve
curve(coef1[4] + (coef1[1] - coef1[4])/(1 + exp(coef1[2]*log(coef1[3]*x))), col="blue", add=TRUE)

#fit model using f2

model2 = nls(form2, data=weeds, start = c(a=5,b=3,g=4))
summary(model2)
coef2 = coef(model2)
coef2
lines(x,f2(x,coef2[1],coef2[2],coef2[3]),col="red")


#use AIC and BIC to determine which model performs better
AIC(model1)
AIC(model2)
BIC(model1)
BIC(model2)
#1st model wins!

#Check assumptions

#1st model

plot(fitted(model1),residuals(model1),xlab="Fitted Values",ylab="Residuals",main= "Plot of the residuals against the fitted values", pch=20, cex=1, col="blue")
abline(a=0, b=0, lty= 3)


# qq-plot
qqnorm(residuals(model1), cex = 1, pch= 20)
qqline(residuals(model1),lty=3,col="blue")
hist(residuals(model1),xlab = 'Residuals', ylab = 'Frequency',main = 'Histogram of residuals')   # not good


shapiro.test(residuals(model1))# H0:normality


#2nd model
plot(fitted(model2),residuals(model2),xlab="Fitted Values",ylab="Residuals",main= "Plot of the residuals against the fitted values", pch=20, cex=1, col="blue")
abline(a=0, b=0, lty= 3)


# qq-plot
qqnorm(residuals(model2), cex = 1, pch= 20)
qqline(residuals(model2),lty=3,col="blue")
hist(residuals(model2),xlab = 'Residuals', ylab = 'Frequency',main = 'Histogram of residuals')   # not good


shapiro.test(residuals(model2))# H0:normality

#new models with deleted parameters

#1st model with deleted d

form3=as.formula(Number~(a)/(1 + exp(b*log(g*Rate))))
f3=function(r,a,b,g)return((a)/(1 + exp(b*log(g*r))))
f4=function(r,a,b)return(a*exp(-b*r))

model3 = nls(form3,data = weeds, start=c(a=20,b=4,g=3))
summary(model3)
coef3 = coef(model3)
coef3

AIC(model3) #better than before
BIC(model3)

plot(fitted(model3),residuals(model3),xlab="Fitted Values",ylab="Residuals",main= "Plot of the residuals against the fitted values", pch=20, cex=1, col="blue")
abline(a=0, b=0, lty= 3)


# qq-plot
qqnorm(residuals(model3), cex = 1, pch= 20)
qqline(residuals(model3),lty=3,col="blue")
hist(residuals(model3),xlab = 'Residuals', ylab = 'Frequency',main = 'Histogram of residuals')   # not good

#2nd model with deleted g

form4=as.formula(Number~a*exp(-b*Rate))
model4 = nls(form4,data = weeds, start=c(a=50,b=4))
summary(model4)
coef4 = coef(model4)
coef4

AIC(model4)
BIC(model4)

plot(fitted(model4),residuals(model4),xlab="Fitted Values",ylab="Residuals",main= "Plot of the residuals against the fitted values", pch=20, cex=1, col="blue")
abline(a=0, b=0, lty= 3)


# qq-plot
qqnorm(residuals(model4), cex = 1, pch= 20)
qqline(residuals(model4),lty=3,col="blue")
hist(residuals(model4),xlab = 'Residuals', ylab = 'Frequency',main = 'Histogram of residuals')


#plot with fitted curves

plot(weeds$Rate,weeds$Number,xlab="Rate of weed (kg/ha)",ylab="Number of healthy crops", main = 'Scatterplot with fitted curves') 
legend('topright', legend = c('Model using f1','Model using f2'), lty=c(1,1),lwd=c(1,1),col=c('blue','red'),bty = 'n')

x=seq(from=0.0,to=0.72,by=0.01)
lines(x,f3(x,coef3[1],coef3[2],coef3[3]),col="blue")

lines(x,f4(x,coef4[1],coef4[2]),col="red")

