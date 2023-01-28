read.csv("heart_failure_clinical_records_dataset.csv")
library(dplyr)
library(gtools)
library(ggplot2)
library(tidyr)
library(caret)
library(ROCR)
library(skimr)
library(tidyverse)
library(skimr)      # skimming data frames
library(ggthemes)
library(patchwork)  # combine separate ggplots into the same graphic
library(corrplot)

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
palette_ro = c("#ee2f35", "#fa7211", "#fbd600", "#75c731", "#1fb86e", "#0488cf", "#7b44ab")
p1 <- ggplot(df, aes(x = anaemia, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels = c("0(False)", "1(True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0(False)", "1(True)")) +
  labs(x = "Anaemia") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
           size = 5,show.legend = FALSE)
p2 <- ggplot(df, aes(x = diabetes, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels = c("0(False)", "1(True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0(False)", "1(True)")) +
  labs(x = "Diabetes") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5,show.legend = FALSE)
p3 <- ggplot(df, aes(x = high_blood_pressure, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels = c("0(False)", "1(True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0(False)", "1(True)")) +
  labs(x = "High blood pressure") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5,show.legend = FALSE)
p4 <- ggplot(df, aes(x = sex, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels = c("0(False)", "1(True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0(False)", "1(True)")) +
  labs(x = "Sex") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5,show.legend = FALSE)

p5 <- ggplot(df, aes(x = smoking, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels = c("0(False)", "1(True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0(False)", "1(True)")) +
  labs(x = "Smoking") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5,show.legend = FALSE)
p6 <- ggplot(df, aes(x = DEATH_EVENT, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels = c("0(False)", "1(True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0(False)", "1(True)")) +
  labs(x = "DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5,show.legend = FALSE)
((p1 + p2 + p3) / (p4 + p5 + p6)) +
  plot_annotation(title = "Distribution of binary features and DEATH_EVENT")
p1 <- ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 5,colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 5))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  scale_x_continuous(breaks = seq(40,100,10)) +
  geom_vline(xintercept = median(df$age, linetype = "longdash", colour = palette_ro[6]) +
               annotate(geom = "text",
                        x = max(df$age)-5, y = 50,
                        label = str_c("Min.  : ", min(df$age),
                                      "\nMedian  : ", median(df$age),
                                      "\nMean  : ", round(mean(df$age), 1),
                                      "\nMax.  : ", max(df$age))) +
               labs(title = "Age Distribution") +
               theme_minimal(base_size = 12)
  