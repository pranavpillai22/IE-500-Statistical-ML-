read.csv("heart_failure_clinical_records_dataset.csv")
library(dplyr)
library(gtools)
library(ggplot2)
library(tidyr)
library(caret)
library(ROCR)
library(skimr)
library(tidyverse)
library(skimr)    
library(ggthemes)
library(patchwork)
library(corrplot)
library(ggcorrplot)
library(Hmisc)

heart <- read.csv("heart_failure_clinical_records_dataset.csv")
skim(heart)
df <- heart %>%
  mutate(sex = factor(sex, levels = c(0,1), labels = c("Female", "Male")),
         anaemia = factor(anaemia, levels = c(0,1), labels = c("No Anaemia", "Anaemia")),
         diabetes = factor(diabetes, levels = c(0,1), labels = c("No Diabetes", "Diabetes")),
         high_blood_pressure = factor(high_blood_pressure, levels = c(0,1),labels = c("No HBP", "BP")),
         smoking = factor(smoking, levels = c(0,1),labels = c("No Smoke", "Smoke")),
         DEATH_EVENT = factor(DEATH_EVENT, levels = c(0,1),labels = c("ALIVE","DEAD")))
glimpse(df)
df <- cor(heart, method = c("spearman"))
corrplot(df, method = "number", type = "lower", tl.col = "red",number.cex = 0.50)
cov(df)
