library(ggalluvial)
library(alluvial)
library(tidyverse)
library(foreign)
library(dplyr)
library(Amelia)

setwd("~/Desktop/learning/Playground/Playground/Autism-Screening-Child-Data Plus Description")
data <- read.arff('Autism-Child-Data.arff')

head(data)

#selected data
data1 <- data[,c(12,13,14,15,17,20,21)]

#rename column
colnames(data1) <- c("Gender", 'Ethnicity', 'Jundice', 'Autism', 'Usee_app_before', 'Relation', "Class_ASD")

#checking missing value and remove it
missmap(data1)
data1.na <- na.omit(data1)

tb <- table(data1.na$Gender,data1.na$Ethnicity,data1.na$Jundice,data1.na$Autism,data1.na$Usee_app_before,data1.na$Relation, data1.na$Class_ASD)
tb <- as.data.frame(tb, stringsAsFactors = FALSE)
colnames(tb) <- c("Gender", 'Ethnicity', 'Jundice', 'Autism', 'Use_app_before', 'Relation',"Class_ASD", 'Freq')

tb$Gender <- recode(tb$Gender, m = 'Male', f = 'Female')

ggplot(tb,
       aes(weight = Freq, axis1 = Gender, axis2 = Use_app_before, axis3 = Jundice, axis4 = Class_ASD)) +
  geom_alluvium(aes(fill = Class_ASD), width = 1/12) +
  geom_stratum(width = 1/12, fill = "green", color = "black") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:4, labels = c("Gender", "Use_app_before", 'Jundice', "Class_ASD")) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Autistic Spectrum Disorder (ASD) by Gender, Jundice and App usage") +
  theme_bw()
