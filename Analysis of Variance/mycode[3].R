data<-read.table("hemoglobin.txt",header=TRUE)

#Two way ANOVA for interaction hypothesis
hemoglobin=as.vector(unlist(data)) 

data$rate = as.factor(data$rate)
data$method = as.factor(data$method)

#contrasts in order to obtain correct parameters
contrasts(data$rate)=contr.sum 
contrasts(data$method)=contr.sum

#full model
model=aov(hemoglobin~rate+method+rate:method,data) 
anova(model)

#obtain model parameters
model$coef
summary(model)
model.tables(model,'means')
model.tables(model)



gm = mean(data$hemoglobin)

#check residuals
plot(model$fitted,model$res,xlab="Fitted Values",ylab="Residuals",main= "Plot of the residuals against the fitted values", pch=20, cex=1, col="blue")
abline(a=0, b=0, lty= 2)

qqnorm(model$res, cex = 1, pch= 20)
qqline(model$res,lty=3,col="blue")
shapiro.test(model$res)

#bartlett.test(split(data$hemoglobin,list(data$rate,data$method,data$rate:data$method))) # H0: homoscedasticity

#check for interaction in the general way

mod1 = aov(hemoglobin~rate+method,data=data)#small model
mod2 = aov(hemoglobin~rate*method,data=data)#big model
anova(mod1,mod2)

#HAB has not been rejected so I create additive model to obtain new parameters

additive = aov(hemoglobin~rate+method,data=data)
anova(additive)

#obtain new parameters

additive$coef
summary(additive)
model.tables(additive,'means')
model.tables(additive)


#check assumptions
plot(additive$fitted,additive$res,xlab="Fitted Values",ylab="Residuals",main= "Plot of the residuals against the fitted values", pch=20, cex=1, col="blue")
abline(a=0, b=0, lty= 2)

qqnorm(additive$res, cex = 1, pch= 20)
qqline(additive$res,lty=3,col="blue")
shapiro.test(additive$res)

#one bartlett test for factor A, one for factor B, and one for the interaction
#bartlett.test(split(data$hemoglobin,list(data$rate,data$method))) # H0: homoscedasticity
bartlett.test(hemoglobin~rate,data=data) #bartlett for factor rate
bartlett.test(hemoglobin~method,data=data)#bartlett for factor method
bartlett.test(split(data$hemoglobin,list(data$rate,data$method)))#bartlett for interaction




