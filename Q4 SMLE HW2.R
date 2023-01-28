#Q4 HW2 IE 500 Statistical Machine Learning Engine 

#call data 

framingham<- read.csv(file = "framingham.csv", header = T)
male<- framingham[,1]
age<- framingham[,2]
education<- framingham[,3]
cuurentSmoker<- framingham[,4]
cigsPerDay<- framingham[,5]
BPMeds<- framingham[,6]
prevalentStroke<- framingham[,7]
prevalentHyp<- framingham[,8]
diabetes<- framingham[,9]
totChol<- framingham[,10]
sysBP<- framingham[,11]
diaBP<- framingham[,12]
BMI<- framingham[,13]
heartRate<- framingham[,14]
glucose<- framingham[,15]

summary(framingam1)
par(mfrow = c(4,2))

#Deleting missing values and creating a new dataset after deletion

framingam1<- na.omit(framingham)

# Data visualization for risk factors 

hist(framingam1$male)
hist(framingam1$age)
hist(framingam1$education)
hist(framingam1$currentSmoker)
hist(framingam1$cigsPerDay)
hist(framingam1$BPMeds)
hist(framingam1$prevalentStroke)
hist(framingam1$prevalentHyp)
hist(framingam1$totChol)
hist(framingam1$sysBP)
hist(framingam1$diaBP)
hist(framingam1$BMI)
hist(framingam1$heartRate)
hist(framingam1$glucose)
hist(framingam1$TenYearCHD)
 
#Multiple linear regression - Q4 Part 3 

heart_study <- lm(framingam1$TenYearCHD~framingam1$male + framingam1$age + framingam1$education 
                  +framingam1$currentSmoker +framingam1$cigsPerDay + framingam1$BPMeds +
                    framingam1$prevalentStroke +framingam1$prevalentHyp + framingam1$totChol 
                  +framingam1$sysBP +framingam1$diaBP + framingam1$BMI + framingam1$heartRate 
                  +framingam1$glucose, data = framingam1) 
summary(heart_study)

                  