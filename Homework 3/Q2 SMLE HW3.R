# Hw 3 SMLE Q2 part 1 

library(tidyverse)
library(caret)
library(glmnet)
#call data and renaming columns
hw3_question2 <- read.table("C:/Users/ppill/Desktop/R files/grocery.txt",header= FALSE)
names(hw3_question2) <- c('Y','X1','X2','X3')

#multiple linear regression
lmfit2 <- lm(Y~ X1+X2+X3, data = hw3_question2)
summary(lmfit2)

#Q2 part 2
#predict data 
z1 <- data.frame(hw3_question2$X1*hw3_question2$X2)
z2 <- data.frame(hw3_question2$X1*hw3_question2$X3)
z3 <- data.frame(hw3_question2$X2*hw3_question2$X3)
z4 <- data.frame(rnorm(52,30,30))
z5 <- data.frame(rnorm(52,7,1))

hw3_question2$z1 <- z1
hw3_question2$z2 <- z2
hw3_question2$z3 <- z3
hw3_question2$z4 <- z4
hw3_question2$z5 <- z5

#lasso regression

a <- hw3_question2$Y
b <- data.frame(hw3_question2$X1,hw3_question2$X2,hw3_question2$X3,hw3_question2$z1,
                hw3_question2$z2,hw3_question2$z3,hw3_question2$z4,hw3_question2$z5)

b_new <- as.matrix(b)

set.seed(123)
lambda <- 10^seq(-3,3, length(100))
cv_model <- cv.glmnet(b_new,a,alpha = 1,lambda = lambda ,nfolds = 5)
optimal_lambda <-cv_model$lambda.min 
optimal_lambda 

plot(cv_model)

#determine best coefficients of best model 

optimal_model <- glmnet(b_new,a, alpha =1, lambda = optimal_lambda)
coef(optimal_model)

#no coefficient is shown for the predictors z1,z2 and z4 as the lasso regression.... 
#....has shrunk the coefficient all the way to zero. As the result these predictor
#variables were left from the model as it does not have much influence on it. 
#source : https://www.statology.org/lasso-regression-in-r/ 

#determine sst and sse 
a_predicted <- predict(optimal_model, s = optimal_lambda, newx = b_new)
sst <- sum((a - mean(a))^2)
sse <- sum((a_predicted-a)^2)
#find r squared 

rsq <- 1 - sse/sst
rsq


