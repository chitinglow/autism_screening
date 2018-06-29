setwd("~/Desktop/learning/Playground/Playground/Autism-Screening-Child-Data Plus Description")
library(mlr) #machine learning
library(foreign) #reading arff file
library(Amelia) #checking missing values
library(tidyverse) #ggplots
library(dplyr) #data manipulation
library(knitr) #for pretty table 
library(PerformanceAnalytics) #for correlation
library(corrr) #for correlation ntework
library(rpart.plot)

set.seed(1234) #reproducible research
#reading data
data <- read.arff('Autism-Child-Data.arff')

#plot missing values
missmap(data)

#remove missing values
data.na <- na.omit(data)

#overview of data
kable(data.na)

names(data.na)
#rename variables
colnames(data.na) <- c("A1_Score", "A2_Score", "A3_Score", "A4_Score", "A5_Score", "A6_Score", "A7_Score",      "A8_Score", "A9_Score", "A10_Score", "age", "gender", "ethnicity", "jundice", "austim", "contry_of_res", "used_app_before", "result", "age_desc", "relation", "Class_ASD")

#summarize of the data
kable(summarizeColumns(data.na))

#attahe the file
attach(data.na)

#recode the varialbe
data.na$gender <- recode(data.na$gender, m = 'Male', f = 'Female')
data.na$Class_ASD <- recode(data.na$Class_ASD, YES = 'Yes', NO = 'No')
data.na$austim <- recode(data.na$austim, yes = 'Yes', no = 'No')
data.na$jundice <- recode(data.na$jundice, yes = 'Yes', no = 'No')

#plot the data for exploratory analysis
autism <- ggplot(data.na, aes(x = Class_ASD, fill = austim)) + geom_bar(stat = 'count', position = 'dodge')
autism + labs(x = 'Class/ASD', y = 'Frequency') + guides(fill = guide_legend(title = "Autism"))

gender <- ggplot(data.na, aes(x = Class_ASD, fill = gender)) + geom_bar(stat = 'count', position = 'dodge')
gender + labs(x = 'Class/ASD', y = 'Frequency') + guides(fill = guide_legend(title = "Gender"))

gender_austim <- ggplot(data.na, aes(x = austim, fill = gender)) + geom_bar(stat = 'count', position = 'dodge')
gender_austim + labs(x = 'Austim', y = 'Frequency') + guides(fill = guide_legend(title = 'Gender'))

jundice <- ggplot(data.na, aes(x = jundice, fill = Class_ASD)) + geom_bar(stat = 'count', position = 'dodge')
jundice + labs(x = 'Jundice', y = 'Frequency') + guides(fill = guide_legend(title = "Class/ASD"))

#show the total number of ethnicity
eth = table(data.na$ethnicity)
kable(eth)


ethnicity <- ggplot(data.na, aes(x = ethnicity, fill = Class_ASD)) + geom_bar(stat = 'count', position = 'dodge')
ethnicity + labs(x = 'Class/ASD', y = 'Frequency') + guides(fill = guide_legend(title = "Autism")) + theme(axis.text.x = element_text(angle = 25)) 


#selecting relevant variable
data.selected <- data.na[,c(1:12,14,15,17,18,20,21)]

data.selected$relation <- as.character(data.selected$relation)
data.selected$relation[data.selected$relation == 'Health care professional'] <- 0
data.selected$relation[data.selected$relation == 'Parent'] <- 1
data.selected$relation[data.selected$relation == 'Relative'] <- 2
data.selected$relation[data.selected$relation == 'self'] <- 3
data.selected$relation[data.selected$relation == 'Self'] <- 3


#recode the factor variable
data.selected$gender <- recode(data.selected$gender, Male = 0, Female = 1)
data.selected$jundice <- recode(data.selected$jundice, No = 0, Yes = 1)
data.selected$austim <- recode(data.selected$austim, No = 0, Yes = 1)
data.selected$used_app_before <- recode(data.selected$used_app_before, no = 0, yes = 1)
data.selected$Class_ASD <- recode(data.selected$Class_ASD, No = 0, Yes = 1)

#unfactor data
data.selected$A1_Score <- as.numeric(data.selected$A1_Score)
data.selected$A2_Score <- as.numeric(data.selected$A2_Score)
data.selected$A3_Score <- as.numeric(data.selected$A3_Score)
data.selected$A4_Score <- as.numeric(data.selected$A4_Score)
data.selected$A5_Score <- as.numeric(data.selected$A5_Score)
data.selected$A6_Score <- as.numeric(data.selected$A6_Score)
data.selected$A7_Score <- as.numeric(data.selected$A7_Score)
data.selected$A8_Score <- as.numeric(data.selected$A8_Score)
data.selected$A9_Score <- as.numeric(data.selected$A9_Score)
data.selected$A10_Score <- as.numeric(data.selected$A10_Score)
data.selected$relation <- as.numeric(data.selected$relation)
data.selected$Class_ASD <- as.numeric(data.selected$Class_ASD)

#correlation
chart.Correlation(data.selected, histogram = T, cex = 30)

#correlation network
data.selected %>% correlate() %>% network_plot(colors = 'red')

#factor data
data.selected$A1_Score <- as.factor(data.selected$A1_Score)
data.selected$A2_Score <- as.factor(data.selected$A2_Score)
data.selected$A3_Score <- as.factor(data.selected$A3_Score)
data.selected$A4_Score <- as.factor(data.selected$A4_Score)
data.selected$A5_Score <- as.factor(data.selected$A5_Score)
data.selected$A6_Score <- as.factor(data.selected$A6_Score)
data.selected$A7_Score <- as.factor(data.selected$A7_Score)
data.selected$A8_Score <- as.factor(data.selected$A8_Score)
data.selected$A9_Score <- as.factor(data.selected$A9_Score)
data.selected$A10_Score <- as.factor(data.selected$A10_Score)
data.selected$gender <- as.factor(data.selected$relation)
data.selected$jundice <- as.factor(data.selected$relation)
data.selected$austim <- as.factor(data.selected$relation)
data.selected$used_app_before <- as.factor(data.selected$relation)
data.selected$relation <- as.factor(data.selected$relation)
data.selected$Class_ASD <- as.factor(data.selected$Class_ASD)


#machine learning classification
#spliting data
n = nrow(data.selected)
train.set = sample(n, size = 2/3*n)
test.set = setdiff(1:n, train.set)

#making ml task
classif.task <- makeClassifTask(data = data.selected, target = 'Class_ASD')

#using decision tree algorithm
lrn <- makeLearner('classif.randomForest', predict.type = 'prob')

#train the model
model <- train(lrn, classif.task, subset = train.set)

#predict
pred <- predict(model, classif.task, subset = test.set)

#performance of prediction
performance <- performance(pred, measures = list(fpr, tnr, mmce, acc, mcc))
performance

df = generateThreshVsPerfData(pred, measures = list(fpr, tpr, mmce, acc))
plotThreshVsPerf(df)
plotROCCurves(df)

calculateConfusionMatrix(pred)


