library(fda)
library(caret)
library(glmnet)

ecg_data_train = read.csv("C:/Users/ppill/Desktop/R files/ECG200TRAIN.csv",
                          header=F, sep=',', na.strings=c('.','NA','99999999'))

ecg_data_test = read.csv("C:/Users/ppill/Desktop/R files/ECG200TEST.csv",
                         header=F, sep=',', na.strings=c('.','NA','99999999'))

#Data Manipulation to get matrix form for analysis

head(ecg_data_train)
ecg_data_train_matrix <- matrix(data = unlist(ecg_data_train), nrow = 100)
ecg_data_test_matrix <- matrix(data = unlist(ecg_data_test), nrow = 100)

y = ecg_data_train_matrix[,1]
x = ecg_data_train_matrix[,2:97]
y[y ==-1] = 0

y_test = ecg_data_test_matrix[,1 ]
x_test = ecg_data_test_matrix[,2:97]
y_test[y_test ==-1] = 0

argvals = seq(0,1, length.out = dim(x)[2])

#Generating Spline Basis for training data

nbasis = 12
bbasis = create.bspline.basis(c(0,1), nbasis)

train_Coef = matrix(0,dim(x)[1],nbasis) 


for (i in 1:dim(x)[1]) {
  train_Coef[i,] = smooth.basis(argvals = argvals, 
                                y = as.numeric(x[i,]), 
                                fdParobj = bbasis)$fd$coefs
}

#Generating Basis for Testing Data

argvals = seq(0,1, length.out = dim(x_test)[2])
nbasis = 24
bbasis = create.bspline.basis(c(0,1), nbasis)



test_Coef = matrix(0,dim(x_test)[1],nbasis) 
for (i in 1:dim(x_test)[1]) {
  test_Coef[i,] = smooth.basis(argvals = argvals, 
                               y = as.numeric(x_test[i,]), 
                               fdParobj = bbasis)$fd$coefs
}

#Decomposing the coefficient for test and training datasets

test_Coef <- as.data.frame(test_Coef)
train_Coef <- as.data.frame(train_Coef)

tdata_x <- test_Coef
tdata_y <-  y_test

#Fitting the decomposed model to a Logistic Regression

ecg_data_train[[1]] <- gsub(-1,0, ecg_data_train[[1]])
ecg_data_train[[1]] <- as.factor(as.numeric(ecg_data_train[[1]]))
train_Coef$y<- ecg_data_train[[1]]

m0_log <- glm(y~ ., family = 'binomial', data = train_Coef)
pred_m0 <- predict(m0_log, test_Coef, type = 'response')
pred_m0

confusionMatrix(data = as.factor(as.numeric(pred_m0>0.5)), reference = as.factor(y))