library(car)
library(ggplot2)


data=read.table("fiber.txt",header=TRUE)

#one-way ANOVA to check the machine influence, without taking thickness into account

strength=as.vector(unlist(data)) 
thickness=as.vector(data$thickness)
#contrasts in order to obtain correct parameters
data$type = as.factor(data$type)
contrasts(data$type)=contr.sum 

model1 = aov(strength~type,data = data) #-1 in order to take correct parameter estimates
anova(model1)
summary(model1)
model.tables(model1)
model1$coef



#check residuals and model assumptions

plot(model1$fitted,model1$res,xlab="Fitted Values",ylab="Residuals",main= "Plot of the residuals against the fitted values", pch=20, cex=1, col="blue")
abline(a=0, b=0, lty= 2)

qqnorm(model1$res, cex = 1, pch= 20)
qqline(model1$res,lty=3,col="blue")
shapiro.test(model1$res)# H0:normality

bartlett.test(strength~type,data=data)# H0: homoscedasticity
leveneTest(strength~type,data=data)


#ANCOVA with one factor and one covariate(thickness)

model2 = aov(strength~thickness+type,data=data)#factor second!
anova(model2)
model.tables(model2,'means')
model.tables(model2)

#purpose of this model is to obtain the correct parameter estimates 
model5 = aov(strength~-1+thickness + type,data=data) #-1 to obtain correct coefficients(to remove mu)

anova(model5)
model.tables(model5,'means')
model.tables(model5)
model5$coef

model3 = aov(strength~type + thickness,data=data)#covariate second
anova(model3)
model.tables(model3,'means')
model.tables(model3)
model3$coef

avg_thick = mean(data$thickness)
#estimated fiber strength for fiber with average thickness 
y1 = model5$coefficients[2] + model5$coefficients[1] * (avg_thick)
y1
y2 = model5$coefficients[3] + model5$coefficients[1] * (avg_thick)
y2
y3 = model5$coefficients[4] + model5$coefficients[1] * (avg_thick))
y3

#using drop1
drop1(model2,test="F")

#check residuals for model2

plot(model2$fitted,model2$res,xlab="Fitted Values",ylab="Residuals",main= "Plot of the residuals against the fitted values", pch=20, cex=1, col="blue")
abline(a=0, b=0, lty= 2)

qqnorm(model2$res, cex = 1, pch= 20)
qqline(model2$res,lty=3,col="blue")
shapiro.test(model2$res)# H0:normality




plot(data$strength~data$thickness,pch=as.character(data$type))

#Compare slopes visually
ggplot(data, aes(thickness, strength,color=type))+
geom_point()+
stat_smooth(method="lm",se=FALSE)+
ggtitle('Dependence of fiber strength on thickness for each machine')


#check with model
model4 = aov(strength~thickness*type,data=data)
anova(model4)
model.tables(model4,'means')
model.tables(model4)

#plot with slopes
#library(HH)
#ancova(strength ~ thickness + type, data=data)

#model4 = aov(strength~)

