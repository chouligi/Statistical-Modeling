library(forecast)


data = sunspot.month

plot(data, main = 'Observed sunspots from 1749 to 1997', xlab = 'Time', ylab = 'Montly count')
acf(data,main = 'ACF as a function of lags')
x.acf = acf(data,main = 'ACF as a function of lags',lag.max = 300)
acf(data,main = 'ACF as a function of lags',lag.max = 300)

plot(x.acf,xaxt = 'n')
axis(1, seq(0, length(x.acf$acf), 11))
abline(1,0)

#plot a smaller part to identify seasonality
plot(window(data,start = 1900,end = 1944),
     main = 'Observed sunspots from 1900 to 1944',
     ylab = 'Montly count')

decomp = decompose(data)

decomp2 = decompose(window(data,start = 1900,end = 1944))


#remove trend using decompose

decomp_all = decompose(data)
trend1 = decomp_all$trend
plot(trend1, main = 'Trend', ylab='Monthly Number of Sunspots')

stat = decomp_all$random
plot(stat, main = 'Residual Time series',ylab ='Detrended and Deseasonalized Sunspots')



#check residuals
Box.test(stat,type="Box-Pierce")

y = seq(from = 0, to = 1,by = 0.05)
box = matrix(300)
ljung = matrix(300)
for (i in c(1:300)){
  box[i] = Box.test(stat,type="Box-Pierce", lag =i)$p.value # the Portmanteau test
  ljung[i] =Box.test(stat,type="Ljung-Box", lag =i)$p.value # the Portmanteau test
}

plot(ljung, col = 'red', main = 'Portmanteau test',type='l',ylab = 'p-value',xlab = 'lag',ylim = c(0,1),yaxt='n')
lines(box, col = 'blue', main = 'Box-Pierce test',type='l')
axis(side = 2,at = y)
legend('bottomright',
       legend = c('Ljung-Box','Box-Pierce'),lwd =4, lty =c(1,1),cex = 0.7,bty = 'n',
       col = c('red','blue'))
abline(0.05,0,lty=3)




#spline smoothing
par(mfrow=c(3,1))
plot.ts(data,ylab="Sunspots",main="spar=0.9")
SM9=smooth.spline(data,spar=0.9)
lines(SM9,col="red",lwd=2)
plot.ts(data,ylab="Sunspots",main="spar=0.5")
SM5=smooth.spline(data,spar=0.5)
lines(SM5,col="red",lwd=2)
plot.ts(data,ylab="Sunspots",main="spar=0.1")
SM1=smooth.spline(data,spar=0.1)
lines(SM1,col="red",lwd=2)



#spline smoothing
par(mfrow=c(3,1))
plot.ts(data,ylab="Sunspots",main="spar=0.4")
SM4=smooth.spline(data,spar=0.4)
lines(SM4,col="red",lwd=2)
plot.ts(data,ylab="Sunspots",main="spar=0.35")
SM35=smooth.spline(data,spar=0.35)
lines(SM35,col="red",lwd=2)
plot.ts(data,ylab="Sunspots",main="spar=0.3")
SM3=smooth.spline(data,spar=0.3)
lines(SM3,col="red",lwd=2)


detrended = data - SM35$y
acf(detrended, main = 'ACF', ylab = 'Autocorrelation')
pacf(detrended, main = 'PACF',ylab= 'Partial Autocorrelation')
plot(detrended, main = 'Residual Time series', xlab = 'Time', ylab = 'Detrended and Deseasonalized Sunspots')
Box.test(detrended,type="Box-Pierce")
Box.test(detrended,type="Ljung-Box")




