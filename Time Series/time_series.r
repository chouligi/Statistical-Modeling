## population in the US in millions
population=scan("population.txt")/10^6
year=seq(1790,2000,by=10)
plot(year,population,t="b",pch=20,lty=1,xlab="Year",ylab="Population (in millions)",main="U.S. Population")

## Accidental deaths in the US
accidents=scan("accidents.txt")
plot(1:72,accidents,t="l",xlab="Month in period from 1973 to 1978",
     ylab="Number of accindental deaths",main="Accidental Deaths in the U.S.")
abline(v=c(12,24,36,48,60),lty=3)


## Trend removal for the time series US population
yearsq=year^2
trend=lm(population~year+yearsq); trend$coef
plot(year,population,t="b",pch=20,lty=1,xlab="Year",
     ylab="Population (in millions)",main="U.S. Population")
lines(year,trend$coef[1]+trend$coef[2]*year+trend$coef[3]*yearsq,col="blue")
pop.detr=population-(trend$coef[1]+trend$coef[2]*year+trend$coef[3]*yearsq)
plot(year,pop.detr,t="l",xlab="Time",ylab=" Detrended population (in millions)")

## Seasonability removal for the accidental death time series
des.accidents=diff(accidents,lag=12)
plot(13:72,des.accidents,t="l",xlab="Month in period from 1973 to 1978",
     ylab="Deseasonalized number of accindental deaths")


# The multivariate time series EuStockMarkets (included in the 
# standard R-installation) gives the daily closing prices of four major 
# European stock indices (German DAX, Swiss SMI, French CAC and British FTSE) 
# from 1991 to 1998, in business time (excludes weekends and holidays).   
# Plot data
plot(EuStockMarkets)



par(mfrow=c(3,1))
plot.ts(sunspots[1:1000],ylab="Sunspots",main="spar=0.8")
SM=smooth.spline(1:1000,sunspots[1:1000],spar=0.8)
lines(SM,col="red",lwd=2)
plot.ts(sunspots[1:1000],ylab="Sunspots",main="spar=0.5")
SM=smooth.spline(1:1000,sunspots[1:1000],spar=0.5)
lines(SM,col="red",lwd=2)
plot.ts(sunspots[1:1000],ylab="Sunspots",main="spar=0.2")
SM=smooth.spline(1:1000,sunspots[1:1000],spar=0.2)
lines(SM,col="red",lwd=2)

par(mfrow=c(1,1))
plot.ts(sunspots[1:1000],ylab="Sunspots",main="spar=0.5")
SM=smooth.spline(1:1000,sunspots[1:1000],spar=0.5)
lines(SM,col="red",lwd=2)

Residuals=sunspots[1:1000]-SM$y
plot.ts(Residuals)


par(mfrow=c(1,3))

ma.model=list(ma=c(0.7,-1.2))
ma=arima.sim(100,model=ma.model,sd=sqrt(0.16))
plot(ma,ylab="", main="MA(2), b1=0.7,b2=-1.2,sgm=0.4")

ar.model=list(ar=-0.9)
ar=arima.sim(100,model=ar.model,sd=sqrt(0.81))
plot(ar,ylab="",main="AR(1), a1=-0.9,sgm=0.9")

arma.model=list(ar=c(-0.1,0.8),ma=1.4)
arma=arima.sim(100,model=arma.model,sd=sqrt(2.25))
plot(arma,ylab="",main="ARMA(2,1), a1=-0.1,a2=0.8,b1=1.4,sgm=1.5")

par(mfrow=c(1,1))

acf(ma)
ma.mle=arima(ma,order=c(0,0,2),method="ML",include.mean=F)
ma.mle
Box.test(resid(ma.mle),type="Ljung-Box")$p.value 
#Box.test(ma,type="Ljung-Box")$p.value 

acf(ar,type="partial") #PACF
ar.mle=arima(ar,order=c(1,0,0),method="ML",include.mean=F)
ar.mle # this is ML estimate of parameter of AR model
# Yule-Walker estimates (better then MLE?)
ar(ar) # or ar(ar,order.max=1), the same as ar.yw(ar,order.max=1)


arma.mle=arima(arma,order=c(2,0,1),method="ML",include.mean=F)
arma.mle
# Sample ACF of the residuals
acf(resid(arma.mle), main="Sample ACF of the residuals")
Box.test(resid(arma.mle),type="Box-Pierce")$p.value # the Portmanteau test
# Box.test(arma,type="Box-Pierce")$p.value
