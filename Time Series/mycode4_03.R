library(forecast)
library(xtable)

data = EuStockMarkets

y=seq(from=0,to=8000,by=1000)


plot.ts(data)

plot.ts(data[,1], main = 'Daily closing prices of stock indices',ylim=c(0,9000), ylab ='Price', yaxt ='n')
axis(side =2,at=y)
lines(data[,2],col = 'red')
lines(data[,3],col = 'green')
lines(data[,4],col = 'blue')
legend('topleft',
       legend = c('DAX', 'SMI','CAC','FTSE'),lwd =4, lty =c(1,1),cex = 0.7,bty = 'n',
       col = c('black','red','green','blue'))
acf(data)

ccf(data[,1],data[,2], main = 'DAX and SMI',type = 'correlation',lag.max = 800)
ccf(data[,1],data[,3], main = 'DAX and CAC')
ccf(data[,1],data[,4], main = 'DAX and FTSE')
ccf(data[,2],data[,3], main = 'SMI and CAC')
ccf(data[,2],data[,4], main = 'SMI and FTSE')
ccf(data[,3],data[,4], main = 'CAC and FTSE')

c1 = ccf(data[,1],data[,2],main = 'DAX and SMI',lag.max = 800)
max1 = max(c1$acf)
lag1 = c1$lag[max(c1$acf)]

c2 = ccf(data[,1],data[,3],main = 'DAX and CAC',lag.max = 800)
max2 = max(c2$acf)
lag2 = c2$lag[which.max(c2$acf)]

c3 = ccf(data[,1],data[,4],main = 'DAX and FTSE',lag.max = 800)
max3 = max(c3$acf)
lag3 = c3$lag[which.max(c3$acf)]

c4 = ccf(data[,2],data[,3], main = 'SMI and CAC',lag.max = 800)
max4 = max(c4$acf)
lag4 = c4$lag[which.max(c4$acf)]

c5 = ccf(data[,2],data[,4], main = 'SMI and FTSE',lag.max = 800)
max5 = max(c5$acf)
lag5 = c5$lag[which.max(c5$acf)]

c6 = ccf(data[,3],data[,4],main = 'CAC and FTSE',lag.max = 800)
max6 = max(c6$acf)
lag6 = c6$lag[which.max(c6$acf)]

max1
max2
max3
max4
max5
max6

plot(c1, main = 'DAX and SMI', type ='l')
plot(c2, main = 'DAX and CAC', type ='l')
plot(c3, main = 'DAX and FTSE', type ='l')
plot(c4, main = 'SMI and CAC', type ='l')
plot(c5, main = 'SMI and FTSE', type ='l')
plot(c6, main = 'CAC and FTSE', type ='l')











