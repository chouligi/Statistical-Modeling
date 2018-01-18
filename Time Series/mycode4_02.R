library(forecast)
library(xtable)
library(FitAR)

n = 500
p=2
q=1

var = 2


ts=list(ar=c(-0.2,0.7),ma=0.8)
arma=arima.sim(n,model=ts,sd=sqrt(var))
plot(arma,main="ARMA(2,1) Time Series",ylab = 'Xt')


fit=arima(arma,order=c(p,0,q),include.mean=F)

#95% confidence interval
confint(fit)


summary(fit)
xtable(summary(fit))


residuals = fit$residuals
plot(residuals, main ='Residuals of ARMA(2,1)', ylab = 'Residuals')
Box.test(residuals,type="Box-Pierce")$p.value # the Portmanteau test
Box.test(residuals,type="Ljung-Box")$p.value # the Portmanteau test

y = seq(from = 0, to = 1,by = 0.05)
box = matrix(300)
ljung = matrix(300)
for (i in c(1:300)){
  box[i] = Box.test(residuals,type="Box-Pierce", lag =i)$p.value # the Portmanteau test
  ljung[i] =Box.test(residuals,type="Ljung-Box", lag =i)$p.value # the Portmanteau test
}

plot(ljung, col = 'red', main = 'Portmanteau test',type='l',ylab = 'p-value',xlab = 'lag',ylim = c(0,1),yaxt='n')
lines(box, col = 'blue', main = 'Box-Pierce test',type='l')
axis(side = 2,at = y)
legend('bottomright',
       legend = c('Ljung-Box','Box-Pierce'),lwd =4, lty =c(1,1),cex = 0.7,bty = 'n',
       col = c('red','blue'))
abline(0.05,0,lty=3)

acf(residuals, main = 'ACF of ARMA(2,1) Residuals')
pacf(residuals, main = 'PACF of ARMA(2,1) Residuals')


###AR MODEL fitted in the same ARMA time series


fitar=ar(arma, order.max=p, method = c('yule-walker'))
resAR = fitar$resid
b1=Box.test(resAR,type="Box-Pierce")$p.value # the Portmanteau test

fit.ar = arima(arma, order = c(p,0,0), include.mean = FALSE, init = fitar$ar)
summary(fitar)
fitar


##Create AR time series and estimate again

ts2=list(ar=c(-0.2,0.7))
ar=arima.sim(n,model=ts2,sd=sqrt(var))
plot(ar,main="AR(2) Time Series",ylab = 'Xt')



fitar2=ar(ar, order.max=p, method = c('yule-walker'))
fit.ar = arima(ar, order = c(p,0,0),include.mean = FALSE)
confint(fit.ar)


residuals2 = fitar2$resid

plot(residuals2, main ='Residuals of AR(2) model', ylab = 'Residuals')
Box.test(residuals2,type="Box-Pierce")$p.value # the Portmanteau test
Box.test(residuals2,type="Ljung-Box")$p.value # the Portmanteau test



y = seq(from = 0, to = 1,by = 0.05)
box = matrix(300)
ljung = matrix(300)
for (i in c(1:300)){
  box[i] = Box.test(residuals2,type="Box-Pierce", lag =i)$p.value # the Portmanteau test
  ljung[i] =Box.test(residuals2,type="Ljung-Box", lag =i)$p.value # the Portmanteau test
}

plot(ljung, col = 'red', main = 'Portmanteau test',type='l',ylab = 'p-value',xlab = 'lag',ylim = c(0,1),yaxt='n')
lines(box, col = 'blue', main = 'Box-Pierce test',type='l')
axis(side = 2,at = y)
legend('bottomright',
       legend = c('Ljung-Box','Box-Pierce'),lwd =4, lty =c(1,1),cex = 0.7,bty = 'n',
       col = c('red','blue'))
abline(0.05,0,lty=3)


acf(residuals2, main = 'ACF of AR(2) Residuals',na.action = na.omit)
pacf(residuals2, main = 'PACF of AR(2) Residuals',na.action = na.omit)


