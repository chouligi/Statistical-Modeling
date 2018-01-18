library(MASS)
library(ggplot2)
data  = muscle

#scatterplot of the data
plot(Length ~ Conc ,xlab="CaCl concentration",ylab="Length of muscle section",main= "Scatterplot of the data", pch=20, cex=1, col="blue", data = data)



#fit model

data.fit = nls(Length ~ th1 + th2 * exp(-Conc/th3), data, list(th1 = 10, th2 = -5, th3 = 5))
summary(data.fit)

coef = coef(data.fit)

plot(Length ~ Conc ,xlab="CaCl concentration",ylab="Length of muscle section",main= "Data along with fitted curve", pch=20, cex=1, col="blue", data = data)
#curve(coef[1] + coef[2]*exp(-x/coef[3]),col='red',add=TRUE)
x=seq(from=0.25,to=4,by=0.01)
lines(x,coef[1] + coef[2]*exp(-x/coef[3]),col="red")
res1 = residuals(data.fit)

#Second model

data2.fit = nls(Length ~ th1[Strip] + th2[Strip] * exp(-Conc/th3), data, list(th1 = rep(coef[1],21) , th2 = rep(coef[2],21), th3 = coef[3]))
coef2 = coef(data2.fit)
summary(data2.fit)

res2 = residuals(data2.fit)

#Compare curves of different animals 


#same theta1

x3 = seq(from=1,to=4,by=0.01)
plot(data$Conc[data$Strip == "S16"], data$Length[data$ Strip == "S16"], pch=20, col="blue",ylim=c(2, 35), xlim=c(0, 4), xlab= "CaCl Concentration", ylab = "Length of Muscle section",main = 'Animals with same theta1')
legend('bottomright', legend = c('Animal 16','Animal 17'), lty=c(1,1),lwd=c(1,1),col=c('blue','black'),bty = 'n')
lines(x3, coef2[16] + coef2[37] * exp(-x3 / coef2[43]), col="blue ")


points(data$Conc[data$Strip == "S17"], data$Length[data$ Strip == "S17"], pch=20, col="black")
lines(x3, coef2[17] + coef2[38] * exp(-x3 / coef2[43]), col="black ")

#same theta2
plot(data$Conc[data$Strip == "S02"], data$Length[data$ Strip == "S02"], pch=20, col="red",ylim=c(2, 35), xlim=c(0, 4), xlab= "CaCl Concentration", ylab = "Length of Muscle section", main = 'Animals with same theta2')
legend('bottomright', legend = c('Animal 2','Animal 21'), lty=c(1,1),lwd=c(1,1),col=c('red','green'),bty = 'n')
lines(x3, coef2[2] + coef2[23] * exp(-x3 / coef2[43]), col="red ")

points(data$Conc[data$Strip == "S21"], data$Length[data$ Strip == "S21"], pch=20, col="green")
lines(x3, coef2[21] + coef2[42] * exp(-x3 / coef2[43]), col="green ")

#Check Assumptions
#1st model
plot(fitted(data.fit),res1,xlab="Fitted Values",ylab="Residuals",main= "Plot of the residuals against the fitted values", pch=20, cex=1, col="blue") # not good
abline(a=0, b=0, lty= 2)
hist(res1)
hist(res2)
qqnorm(summary(data.fit)$res, cex = 1, pch= 20)
qqline(summary(data.fit)$res,lty=3,col="blue")


shapiro.test(summary(data.fit)$res)# H0:normality
#2nd Model
plot(fitted(data2.fit),res2,xlab="Fitted Values",ylab="Residuals",main= "Plot of the residuals against the fitted values", pch=20, cex=1, col="blue") # not good
abline(a=0, b=0, lty= 2)


qqnorm(summary(data2.fit)$res, cex = 1, pch= 20)
qqline(summary(data2.fit)$res,lty=3,col="blue")


shapiro.test(summary(data2.fit)$res)# H0:normality


