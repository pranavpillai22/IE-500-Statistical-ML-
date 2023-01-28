#IE 500 SMLE HW 3 Question 3 

library(tidyverse)
library(caret)
library(glmnet)

#load data 
#loading train data 
train_data <- read.csv(file = 'train.air.csv', header = TRUE)
#test data 
test_data <- read.csv(file = 'test.air.csv', header = TRUE)
#rescaling data to 0 and 1
library(scales)
#rescaling test data 
new_testCO <- rescale(test_data$CO)
new_testC6H6 <- rescale(test_data$C6H6)
new_testNMHC <- rescale(test_data$NMHC)
new_testNox <- rescale(test_data$Nox)
new_testNO2 <- rescale(test_data$NO2)
new_testO3 <- rescale(test_data$O3)
new_testT <- rescale(test_data$T)
new_testRH <- rescale(test_data$RH)
new_testAH <- rescale(test_data$AH)

new_test <- data.frame(new_testCO,new_testC6H6,new_testNMHC,new_testNox,new_testNO2
                       ,new_testO3,new_testT,new_testRH,new_testAH)
#rescaling train data 
new_trainCO <- rescale(train_data$CO)
new_trainC6H6 <- rescale(train_data$C6H6)
new_trainNMHC <- rescale(train_data$NMHC)
new_trainNox <- rescale(train_data$Nox)
new_trainNO2 <- rescale(train_data$NO2)
new_trainO3 <- rescale(train_data$O3)
new_trainT <- rescale(train_data$T)
new_trainRH <- rescale(train_data$RH)
new_trainAH <- rescale(train_data$AH)

new_train <- data.frame(new_trainCO,new_trainC6H6,new_trainNMHC,new_trainNox,new_trainNO2
                       ,new_trainO3,new_trainT,new_trainRH,new_trainAH)
#predictor variable 
x<- model.matrix(new_trainCO~., new_train)[,-1]
#outcome variable 
y <- new_train$new_trainCO
#computing ridge regression 
set.seed(123)
cv <- cv.glmnet(x,y, alpha = 0)
#show best lambda value 
cv$lambda.min
#fit final model on training data 
ridge_model <- glmnet(x,y, alpha = 0, lambda = cv$lambda.min)
coef(ridge_model)
#predict test data 
x_test_ridge <- model.matrix(new_testCO~., new_test)[,-1]
prediction_ridge <- ridge_model %>% predict(x_test_ridge) %>% as.vector()
# model performance metrics 
library(Metrics)
data.frame(RMSE_ridge = rmse(prediction_ridge, new_test$new_testCO),
           Rsquared_ridge = R2(prediction_ridge,new_test$new_testCO)
           )
#computing lasso regression
set.seed(123)
cv <- cv.glmnet(x,y, alpha = 1)
#show best lambda value 
cv$lambda.min
#fit final model on training data 
lasso_model <- glmnet(x,y, alpha = 1, lambda = cv$lambda.min)
coef(lasso_model)
#predict test data 
x_test_lasso <- model.matrix(new_testCO~., new_test)[,-1]
prediction_lasso <- lasso_model %>% predict(x_test_lasso) %>% as.vector()
# model performance metrics 
data.frame(
  RMSE_lasso = rmse(prediction_lasso, new_test$new_testCO),
  Rsquare_lasso = R2(prediction_lasso,new_test$new_testCO)
)
#computing elastic net regression
set.seed(123)
cv <- cv.glmnet(x,y, alpha = 0.3)
#show best lambda value 
cv$lambda.min
#fit final model on training data 
elasticnet_model <- glmnet(x,y, alpha = 0.3, lambda = cv$lambda.min)
coef(elasticnet_model)
#predict test data 
x_test_elasticnet <- model.matrix(new_testCO~., new_test)[,-1]
prediction_elasticnet <- elasticnet_model %>% predict(x_test_elasticnet) %>% as.vector()
# model performance metrics 
data.frame(
  RMSE_elasticnet = rmse(prediction_elasticnet, new_test$new_testCO),
  Rsquare_elasticnet = R2(prediction_elasticnet,new_test$new_testCO)
)

#The lasso model will be selected to present the prediction for Carbon Monoxide
#as it has a lower RMSE compared to ridge and elastic net regression

