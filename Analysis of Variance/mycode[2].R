data = chickwts

#Create the boxplot

boxplot(weight~feed,names = c("Casein","Horsebean","Linseed","Meatmeal","Soybean","Sunflower"),
        main = "Boxplots of the distribution of chick weights",xlab = "Feed Supplement",ylab = "Weight (grams)",data = data)


#obtain the number of observations and the levels

n = length(data$weight)
i = length(levels(data$feed))
# obtain the grand mean
gm = mean(data$weight)

#create data frame with 60 observations, 10 for each level

#d_horsebean = subset(data, data$feed == "horsebean")
#d_linseed = subset(data, data$feed == "linseed")
#d_soybean = subset(data, data$feed == "soybean")
#d_sunflower = subset(data, data$feed == "sunflower")
#d_meatmeal = subset(data, data$feed == "meatmeal")
#d_casein = subset(data, data$feed == "casein")

#set.seed(10)
#sample_linseed = d_linseed[sample(1:nrow(d_linseed), 10, replace=FALSE),]
#sample_horsebean = d_horsebean[sample(1:nrow(d_horsebean), 10, replace=FALSE),]
#sample_soybean = d_soybean[sample(1:nrow(d_soybean), 10, replace=FALSE),]
#sample_sunflower = d_sunflower[sample(1:nrow(d_sunflower), 10, replace=FALSE),]
#sample_meatmeal = d_meatmeal[sample(1:nrow(d_meatmeal), 10, replace=FALSE),]
#sample_casein = d_casein[sample(1:nrow(d_casein), 10, replace=FALSE),]

#####combine to a dataframe

#new_data = rbind(sample_linseed,sample_horsebean,sample_soybean,sample_sunflower,sample_meatmeal,sample_casein)

#Obtain Incidence Matrix and Response Variable

response = data$weight
feed = data$feed
X = model.matrix(~ feed - 1) # -1 indicates that we remove the intercept

#check the rank of the matrix to verify that it is of full rank
I = qr(X)$rank #Obtain the rank of matrix

X_T  = t(X) #X_Transpose

#betahat
betaH = solve(X_T %*% X) %*% X_T %*% response #inverse obtained by solve function

#residuals

e_hat = response  - X %*% betaH



#Residuals Sum of Squares

SSE = t(response - X %*% betaH) %*% (response - X %*% betaH)
cat("Residuals Sum of Squares: ",SSE )
wgDf = n - I
meanSSE = SSE/wgDf

cat("Within Groups DF: ", wgDf,"Mean Value: ", meanSSE)

#obtain unbiased estimator of variance

var = SSE / (n-I)



#Between groups sum of squares
#manually
n1=10
n2=12
n3=14
n4=12
n5=11
n6=12

y1=mean(data$weight[1:10])
y2=mean(data$weight[11:22])
y3=mean(data$weight[23:36])
y4=mean(data$weight[37:48])
y5=mean(data$weight[49:59])
y6=mean(data$weight[60:71])


bgSS = n1*(y1-gm)^2+n2*(y2-gm)^2+n3*(y3-gm)^2+n4*(y4-gm)^2+n5*(y5-gm)^2+n6*(y6-gm)^2


cat("Between Groups Sum of Squares: ", bgSS)

bgDf = I - 1
cat("Between Groups DF: ", bgDf)
meanbg = bgSS/bgDf

#total Sum of Squares

TSS = bgSS + SSE

cat("Total Sum of Squares: ", TSS)
#F statistic

f = meanbg/meanSSE
f
cat("F  value: ", f)

#obtain p value to determine influence of factor feed supplement
pv = pf(f, bgDf, wgDf, lower.tail = FALSE, log.p = FALSE)
pv
cat("P value:", pv)

#same analysis using anova function

model = aov(weight~feed,data=data)
anova(model)

#check model assumptions

#normality
fit = X %*% betaH

plot(fit,e_hat,xlab="Fitted Values",ylab="Residuals",main= "Plot of the residuals against the fitted values", pch=20, cex=1, col="blue")
abline(a=0, b=0, lty= 2)

qqnorm(e_hat, cex = 1, pch= 20)
qqline(e_hat,lty=3,col="blue")

shapiro.test(e_hat)

## Bartlett's test of homogeneity (homoscedasticity) of variances
bartlett.test(weight~feed,data=data) # H0: homoscedasticity

