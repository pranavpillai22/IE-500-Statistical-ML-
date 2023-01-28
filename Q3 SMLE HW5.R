# IE 500 SMLE HW 5 Q3 
#part 1
library(dplyr)
library(GauPro)
library(tidyverse)
library(caTools)

wafer <- read.csv("C://Users/ppill/Desktop/R files/Wafer+Data.csv",header = FALSE)
names(wafer) <- c('X','Y','T')
wafer <- na.omit(wafer)

set.seed(200)
sample_pts<- wafer[,1:2]
split_samplewafer <- sample.split(sample_pts, SplitRatio = 0.036)
train_wafer <- subset(sample_pts, split = TRUE)
test_wafer <- subset(sample_pts,split = FALSE)
plot(sample_wafer$X,sample_wafer$Y)
gp_wafer <- GauPro(train_wafer$X,train_wafer$Y)

#part 2

library(Metrics)


lm_wafer <- lm(train_wafer$Y ~train_wafer$X , data = train_wafer)
summary(lm_wafer)
predict_lm <- predict(lm_wafer, data = test_wafer)
rmse(test_wafer$Y,predict_lm)
rmse(train_wafer$Y,predict_lm)
rmse(test_wafer$X,predict_lm)
rmse(train_wafer$X,predict_lm)


