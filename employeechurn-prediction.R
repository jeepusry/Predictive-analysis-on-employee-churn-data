library(readr)
library(plyr)
library(dplyr)
library(caret)
library("caTools")
library("partykit")
library (C50)
library(rpart)
library(e1071)
library(pROC)


MFG10YearTerminationData <- read_csv("C:/Users/jeepu/OneDrive/Desktop/Data analytics penn state/DAAN 881 - Data Driven Decision Making/Project/MFG10YearterminationData.csv")
View(MFG10YearTerminationData)

df <- data.frame(MFG10YearTerminationData)

df <- subset(df, select = -c(gender_short))
df <- data.frame(df)
View(df)


df$STATUS <- as.factor(df$STATUS)

df <- subset(df, select = -c(EmployeeID, recorddate_key, birthdate_key, orighiredate_key, terminationdate_key,
                             city_name, store_name, STATUS_YEAR, BUSINESS_UNIT, termtype_desc))

sampling_vector <- createDataPartition(df$STATUS, p = 0.80, list = FALSE)
train_reg <- df[sampling_vector,]
nrow(train_reg)

test_reg <- df[-sampling_vector,]
nrow(test_reg)

CARTTree <- rpart(STATUS ~., method = "class", data = train_reg)
summary(CARTTree)

CARTTree$variable.importance

plot(CARTTree, uniform = TRUE, main = "Classification of Employee CHURN")
text(CARTTree, use.n = TRUE, all = TRUE, cex = .6)

CARTTree$cptable

##Assess performance
train_predicted <- predict(CARTTree, newdata = train_reg, type = "class")
mean(train_reg$STATUS == train_predicted)
table(actural = train_reg$STATUS, predict = train_predicted)

test_predicted <- predict(CARTTree, newdata = test_reg, type = "class")
mean(test_reg$STATUS == test_predicted)
table(actural = test_reg$STATUS, predict = test_predicted)

# Training - Calculate AUC and ROC
# Create ROC curve
train_roc <- roc(response = train_reg$STATUS, predictor = factor(train_predicted, ordered = TRUE), plot=TRUE)
plot(train_roc, col="red", lwd=3, main="Training - ROC curve")
auc(train_reg$STATUS, predictor = factor(train_predicted, ordered = TRUE))

# Testing - Calculate AUC and ROC
# Create ROC Curve
test_roc <- roc(response = test_reg$STATUS, predictor = factor(test_predicted, ordered = TRUE), plot=TRUE)
plot(test_roc, col="red", lwd=3, main="Testing - ROC curve")
auc(test_reg$STATUS, predictor = factor(test_predicted, ordered = TRUE))
