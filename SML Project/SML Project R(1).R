SML = read.csv('SMLProject.csv')

library(tidyverse)
library(caTools)

is.na(SML)

Age = SML$age
Anaemia = SML$anaemia
Creatinine = SML$creatinine_phosphokinase
Diabetes = SML$diabetes
Ejection = SML$ejection_fraction
HBP = SML$high_blood_pressure
Platelets = SML$platelets
SerumC = SML$serum_creatinine
SerumS = SML$serum_sodium
Sex = SML$sex
Smoking = SML$smoking
Time = SML$time
Death = SML$DEATH_EVENT

par(mfcol=c(2,2))

hist(SML$age)
hist(SML$anaemia)
hist(SML$creatinine_phosphokinase)
hist(SML$diabetes)
hist(SML$ejection_fraction)
hist(SML$high_blood_pressure)
hist(SML$platelets)
hist(SML$serum_creatinine)
hist(SML$serum_sodium)
hist(SML$sex)
hist(SML$smoking)
hist(SML$time)
hist(SML$DEATH_EVENT)


TableAnaemia <- table(SML$anaemia)

barplot(TableAnaemia, xlab = "Anaemia", ylab = "Frequency")
plot(TableAnaemia)
pie(TableAnaemia)


TableAnaemia <- table(SML$anaemia)

barplot(TableAnaemia, xlab = "Anaemia", ylab = "Frequency")
plot(TableAnaemia)
pie(TableAnaemia)

TableD <- table(SML$diabetes)

barplot(TableD, xlab = "Diabetes", ylab = "Frequency")
plot(TableD)
pie(TableD)

TableHBP <- table(SML$high_blood_pressure)

barplot(TableHBP, xlab = "HBP", ylab = "Frequency")
plot(TableHBP)
pie(TableHBP)

TableSex <- table(SML$sex)

barplot(TableSex, xlab = "Sex", ylab = "Frequency")
plot(TableSex)
pie(TableSex)

TableSmoking <- table(SML$smoking)

hist(TableSmoking)
barplot(TableSmoking, xlab = "Smoking", ylab = "Frequency")
plot(TableSmoking)
pie(TableSmoking)

set.seed(1000)
split = sample.split(SML$DEATH_EVENT , SplitRatio = 0.65)

train = subset(SML, split = TRUE)
test = subset(SML, split = FALSE)

SMLLog = glm(SML$DEATH_EVENT ~ ., data = train, family = binomial)
summary(SMLLog)

predictTest = predict(SMLLog, type="response", newdata=test)

z = table(SML$DEATH_EVENT, predictTest > 0.8)
table(z)
