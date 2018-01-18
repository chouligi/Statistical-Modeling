n = 100
mean=0
sd = sqrt(0.5)

th1 = 5
th2 = 90
th3 = 2

theta_real = c(th1,th2,th3)

set.seed(88)
x = runif(n, 0, 3)
e = rnorm(100,mean,sd)

#create vector Y

Y = th1*x + th2/(th3 + x^2) + e 



plot(Y~  x, main = 'Scatterplot of simulated data')
curve(th1*x+(th2/(th3+x^2)), col="blue", add=TRUE)


#fit model

fit = nls(Y ~ theta1*x+(theta2/(theta3+x^2)), start = list(theta1 = 10, theta2 = 30, theta3 = 9))
coef = coef(fit)

#new curve
plot(Y ~  x, main = 'Scatterplot of simulated data with fitted curve')    
legend('topright', legend = c('True curve','Fitted curve'), lty=c(1,1),lwd=c(1,1),col=c('blue','red'),bty = 'n')
#using lines
x1=seq(from=0.0,to=3,by=0.01)
lines(x1,coef[1]*x1 + coef[2]/(coef[3] + x1^2),col="red")#fitted curve
lines(x1,th1*x1 + th2/(th3 + x1^2),col="blue")#true curve
#using curve
curve(th1*x+(th2/(th3+x^2)), col="blue", add=TRUE)
curve(coef[1]*x + (coef[2]/(coef[3]+x^2)),col='red',add=TRUE)

#the estimated covariance matrix
res = residuals(fit)
SSE = sum((res)^2)
p=3
var = SSE/(n-p)
var
cov = vcov(fit)
cov

#real covariance matrix
expr = expression(th1*x + th2/(th3 + x^2))
#compute partial derivatives with respect to th1, th2 and th3 using function 
der1 = eval(deriv(expr, 'th1'))
der2 = eval(deriv(expr, 'th2'))
der3 = eval(deriv(expr, 'th3'))
V =matrix(NA, nrow = 100, ncol =3)

#here we use attr(,'gradient) to obtain the appropriate partial derivatives from deriv function

V =(cbind(attr(der1,'gradient'),attr(der2,'gradient'),attr(der3,'gradient')))
#calculate real covariance matrix
cov_real =  sd^2 * solve((t(V) %*% V))
cov_real


#check residuals



plot(fitted(fit),res,xlab="Fitted Values",ylab="Residuals",main= "Plot of the residuals against the fitted values", pch=20, cex=1, col="blue") # not good
abline(a=0, b=0, lty= 3)


# qq-plot
qqnorm(res, cex = 1, pch= 20)
qqline(res,lty=3,col="blue")
hist(res)   # not good


shapiro.test(res)# H0:normality



#confidence intervals using Asyptotic normality
f=function(x,theta)return(theta[1]*x+(theta[2]/(theta[3]+x^2)))

ub1 = coef[1] + qt(0.99,n-3)*sqrt(cov[1,1])
lb1 = coef[1] - qt(0.99,n-3)*sqrt(cov[1,1])

ub2 = coef[2] + qt(0.99,n-3)*sqrt(cov[2,2])
lb2 = coef[2] - qt(0.99,n-3)*sqrt(cov[2,2])

ub3 = coef[3] + qt(0.99,n-3)*sqrt(cov[3,3])
lb3 = coef[3] - qt(0.99,n-3)*sqrt(cov[3,3])

v1 = c(lb1,ub1)
v2 = c(lb2,ub2)
v3 = c(lb3,ub3)

interval = rbind (v1,v2,v3)
interval


lb=numeric(3);
ub=numeric(3); 
rownames(lb)=names(coef(fit))
for(i in 1:3) {lb[i]=coef(fit)[i]-qt(0.99,n-length(coef(fit)))*sqrt(cov[i,i])
               ub[i]=coef(fit)[i]+qt(0.99,n-length(coef(fit)))*sqrt(cov[i,i])}
ci=cbind(lb,ub); rownames(ci)=names(coef(fit)); ci



#using bootstrap



B=1000 
par.boot=matrix(NA,B,length(coef(fit)))
rownames(par.boot)=paste("B",1:B,sep="")
colnames(par.boot)=names(coef(fit))
e_tilde = res - mean(res)

for(b in 1:B){
  # cat("b = ",b,"\n")
  # Bootstrap samples from centered residuals
  res_tilde=sample(e_tilde,replace=T)
  # Calculate bootstrap values for the response
  yboot=fitted(fit)+res_tilde #Y*_1,...Y*_n
  # Fit model using new response and get bootstrap estimates for parameter 
  modelBoot=nls(yboot ~ theta1*x+(theta2/(theta3+x^2)),start=list(theta1 = 10, theta2 = 30, theta3 = 9))
  # Store estimated (by bootstrap) parameters  
  par.boot[b,]=coef(modelBoot) #\theta*_1,..., \theta*_B
}
# Compute and display bootstrap confidence interval vor the coordinates of theta
lb.boot=2*coef(fit)-apply(par.boot,2,quantile,prob=0.99)
ub.boot=2*coef(fit)-apply(par.boot,2,quantile,prob=0.01)
cbind(lb.boot,ub.boot)


#Confidence interval for the expected value of Y when x = 1

## estimate of the real mean response f(1,theta)
f1=f(1,theta_real)
f1
## confidence interval for the mean response f(1,theta)
# first gradient function (represeted as column)
grad<-function(x,theta){rbind(x,
                              1/(theta[3] + x^2),
                              -theta[2]/(theta[3]+x^2)^2)}
gradvec=grad(1,coef(fit))
se=sqrt(t(gradvec)%*%vcov(fit)%*%gradvec)
lb=f1-qt(0.99,n-3)*se
ub=f1+qt(0.99,n-3)*se
c(lb,ub) # approximate confidence interval for f(1,theta)


## estimates and confidence approximate intervals the mean responses for many x's from (0,3]
x1=seq(from=0.1,to=3,by=0.1)
f2=f(x1,coef(fit))  
grad<-function(x1,theta){rbind(x1,
                              1/(theta[3] + x1^2),
                              -theta[2]/(theta[3]+x1^2)^2)}
gradvec=grad(x1,coef(fit))
se<-sqrt(apply(gradvec,2,function(xx) t(xx)%*%vcov(fit)%*%xx))


#se=sqrt(t(gradvec)%*%vcov(fit)%*%gradvec)
lb=f2-qt(0.99,n-length(coef(fit)))*se
ub=f2+qt(0.99,n-length(coef(fit)))*se
c(lb,ub) 

#plot confidence intervals


plot(Y ~  x, main = 'Plot of Confidence Intervals for many x in (0,3]')    
legend('topright', legend = c('True curve','Fitted curve'), lty=c(1,1),lwd=c(1,1),col=c('blue','red'),bty = 'n')
#using lines
lines(x1,coef[1]*x1 + coef[2]/(coef[3] + x1^2),col="red")#fitted curve
lines(x1,th1*x1 + th2/(th3 + x1^2),col="blue")#true curve
segments(x1,lb,x1,ub,lty=1,lwd =3, col="grey") # confidence intervals







#confidence interval for the mean response f(1,theta) using bootstrap

B=1000 # 1000
par.boot=matrix(NA,B,length(1))
rownames(par.boot)=paste("B",1:B,sep="")
e_tilde = res - mean(res)

for(b in 1:B){
  # cat("b = ",b,"\n")
  # Bootstrap samples from centered residuals
  res_tilde=sample(e_tilde,replace=T)
  # Calculate bootstrap values for the response
  yboot=fitted(fit)+res_tilde #Y*_1,...Y*_n
  # Fit model using new response and get bootstrap estimates for parameter 
  modelBoot=nls(yboot ~ theta1*x+(theta2/(theta3+x^2)),start=list(theta1 = 10, theta2 = 30, theta3 = 9))
  # Store estimated (by bootstrap) parameters  
  #formula = f(1,coef(modelBoot))
  #par.boot[b,]=coef(modelBoot) #\theta*_1,..., \theta*_B  
 # formula1[b] = c(f(1,coef(modelBoot)))
  par.boot[b,] = c(f(1,coef(modelBoot)))
  #formula = matrix(c(formula1,formula2,formula3))
}
lb.boot=2*f1-apply(par.boot,2,quantile,prob=0.99)
ub.boot=2*f1-apply(par.boot,2,quantile,prob=0.01)
cbind(lb.boot,ub.boot)







