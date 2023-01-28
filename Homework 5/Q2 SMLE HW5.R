#IE 500 SMLE HW 5 Q2 
library(tidyverse)
library(caret)
library(e1071)
library(caTools)
library(ggplot2)
library(lattice)
library(tibble)
library(MASS)

faults <- read.table("C:/Users/ppill/Desktop/R files/Faults",header = FALSE,sep = "")
colnames(faults) <- c('X_Minimum','X_Maximum','Y_Minimum','Y_Maximum',
                     'Pixels_Area','X_Perimeter','Y_Perimeter','Sum_of_Luminosity', 
                     'Minimum_of_Luminosity','Maximum_of_Luminosity','Length_of_Conveyer', 
                     'TypeofSteel_A300','TypeofSteel_A400','Steel_Plate_Thickness', 
                     'Edges_Index','Empty_Index','Square_Index','Outside_X_Index',
                     'Edges_X_Index','Egdes_Y_Index','Outside_Global_Index','LogOfAreas', 
                     'Log_X_Index','Log_Y_Index','Orientation_Index','Luminosity_Index',
                     'SigmoidOfAreas','Pastry','Z_Scratch','K_Scratch','Stains','Dirtiness','Bumps', 
                     'Other Faults')
#part 1
set.seed(1029)
#unifying classes into a vector 
for(i in 28:34)
{
  for(j in 1:nrow(faults))
    if(faults[j,i]==1)
      faults[j,i] <- colnames(faults[i])
}

faults <- add_column(faults,0)
colnames(faults)[35] <- c("type")

for(i in 28:34)
{
  for(j in 1:nrow(faults))
    if(faults[j,i]!=0)
      faults[j,35]<- faults[j,i]
}

faults <- faults[,-c(28:34)]
faults[,28] <- as.factor(faults[,28])

#splitting data 
split_faults <- sample.split(faults$type, SplitRatio = 0.5 )
train <- subset(faults, split = TRUE)
test <- subset(faults,split = FALSE)

svm_faults <- svm(faults$type~. , data = train)
svm_faults
summary(svm_faults)
predict_test <- predict(svm_faults, data = test)
summary(predict_test)
test_tab <- table(predict_test,faults$type)
confusionMatrix(test_tab, positive = "Yes")

#neural network 

library(nnet)

nnet_faults <- multinom(faults$type~. , data = train)
nnet_faults
summary(nnet_faults)
predict_testnn <- predict(nnet_faults, data = test)
test_nntab <- table(predict_testnn,faults$type)
confusionMatrix(test_nntab,positive = "Yes")

#random forests 

library(randomForest)

rf_faults <- randomForest(faults$type~. , data = train)
rf_faults
summary(rf_faults)
predict_testrf <- predict(rf_faults,data = test)
test_rftab <- table(predict_testrf,faults$type)
confusionMatrix(test_rftab,positive = "Yes")

#part 2
#splitting data 
library(tidyverse)
library(caret)
library(e1071)
library(caTools)
library(ggplot2)
library(lattice)
library(tibble)
library(MASS)
set.seed(1029)
split_faults2 <- sample.split(faults$type, SplitRatio = 0.7 )
train2 <- subset(faults, split = TRUE)
test2 <- subset(faults,split = FALSE)

svm_faults2 <- svm(faults$type~. , data = train2)
svm_faults2
summary(svm_faults2)
predict_test2 <- predict(svm_faults2,data=test2)
test_tab2 <- table(predict_test2,faults$type)
confusionMatrix(test_tab2,positive = "Yes")


#neural network 

library(nnet)

nnet_faults2 <- multinom(faults$type~. , data = train2)
nnet_faults2
summary(nnet_faults2)
predict_testnn2 <- predict(nnet_faults2, data=test)
test_nntab2 <- table(predict_testnn2,faults$type)
confusionMatrix(test_nntab2,positive = "Yes")


#random forests 

library(randomForest)

rf_faults2 <- randomForest(faults$type~. , data = train2)
rf_faults2
summary(rf_faults2)
predict_testrf2 <- predict(rf_faults2,data = test2)
test_rftab2 <- table(predict_testrf2,faults$type)
confusionMatrix(test_rftab2,positive = "Yes")


