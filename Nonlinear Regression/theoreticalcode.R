n=200
x = numeric(n)
for(i in 1:100){
  x[i]=0
}
for(i in 101:200){
  x[i]=1
}
e = rnorm(200,0,2)

th1=50
th2=5

Y = sin(th1*x) + th1*exp(-th2*x) + e
plot(Y~  x, main = 'Scatterplot of simulated data')
curve(sin(th1*x) + th1*exp(-th2*x), col="blue", add=TRUE)

fit = nls(Y ~ sin(theta1*x) + theta1*exp(-theta2*x), start = list(theta1 = 40, theta2 = 1.46))
summary(fit)
coef = coef(fit)
coef
k=0
for(i in 1:100){
 k =  k + Y[i]
}
mu1 = k/100
mu1 # equal to the theta1!!!!

theta2estimate = -log((2*mean(Y) - mu1 -sin(mu1))/mu1) #equal to theta2!!!
theta2estimate


