
#Q2 HW 2 Statistical Machine Learning Engine IE 500 Special Topics

#Call Data 
data1 <- read.table("C:/Users/ppill/Desktop/R files/airfreight.txt",header = T)
a <- data1[,1]
b <- data1[,2]

#Scatter Plot 
par(mfcol=c(1,1))
plot(a,b)

#Linear Regression
lmfit<- lm(b~a)
summary(lmfit)
abline(coef(lmfit),lty=5)


#Diagnostic (constant var, normality)
par(mfcol=c(2,2))

#constant var
plot(fitted(lmfit), residuals(lmfit),xlab = "Fitted", ylab = "Residuals")
abline(h=0)
plot(fitted(lmfit),abs(residuals(lmfit)),xlab = "Fitted", ylab = "|Residuals|")

#normality 
qqnorm(residuals(lmfit),ylab= "Residuals")
qqline(residuals(lmfit))
hist(residents(lmfit))

shapiro.test(residuals(lmfit))
par(mfcol=c(1,1))

#Q2(2)
coef(lmfit)[1]+coef(lmfit)[2]*1
predict(lmfit, newdata = data.frame(a = 1))

#Q2(3)
#The increase in the expected number of ampules broken when two transfers occur
#relative to one is the slope which is 4.0 broken ampules.

#Q2(4)
amean <- mean(data1$ShipmentRoute)
bmean <- mean(data1$NumberOfAmpules)
predict(lmfit, newdata = data.frame(a = amean))
bmean

#Q2(5)
#Confidence Interval for beta0 and beta1
confint(lmfit)

#Q2(6)
t.test(a,b)
