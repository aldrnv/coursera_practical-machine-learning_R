---
title: "Course 8 - Practical Machine Learning"
author: "Aldreen Venzon"
date: "January 4, 2019"
output: html_document
---

# 1) Overview
This project will use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways using devices such as Jawbone Up, Nike FuelBand, and Fitbit. 

More information here: http://groupware.les.inf.puc-rio.br/har (section on the Weight Lifting Exercise Dataset). They have been very generous in allowing their data to be used for this kind of assignment.

The goal of this project is to predict the manner in which the participants did the exercise. This is the "classe" variable in the training set. 


# 2) Processing and Cleaning the Data
## Load the data
```{r, ECHO=FALSE}
#getwd()
train <- read.csv("pml-training.csv", header = TRUE, na.strings = c("","NA", "#DIV/0!"))
test <- read.csv("pml-testing.csv", header = TRUE, na.strings = c("","NA", "#DIV/0!"))
```

## Look at data and summarize
```{r, ECHO=FALSE}
# View(train)
# View(test)
dim(train)
dim(test)
```

## Look for and remove columns with a lot of NAs
```{r, ECHO=FALSE}
NA_Data <- round(colMeans(is.na(train)), 3)
# View(NA_Data)
Complete_Col <- which(NA_Data == 0)[-1]
```

## Subset complete data withoug NAs
```{r, ECHO=TRUE}
train <- train[ ,Complete_Col]
test <- test[ ,Complete_Col]
# View(train)
# View(test)
```

## Remove additional variables (for ID purposes only variables) and convert to numeric
```{r, ECHO=TRUE}
train <- train[ , -(1:6)]
test <- test[ , -(1:6)]
for(t in 1:(length(train)-1))
{
    train[ ,t] <- as.numeric(train[ ,t])
    test[ ,t] <- as.numeric(test[ ,t])
}
# View(train)
# View(test)
```


# 3) Model Building for Prediction
## a. Split Train dataset into 70% train and 30% validation
```{r, ECHO=FALSE}
library(caret)
split_Train <- createDataPartition(y = train$classe, p = 0.7, list= FALSE)
train_Train <- train[split_Train,]
train_Validate <- train[-split_Train,]
dim(train)
dim(test)
dim(train_Train)
dim(train_Validate)
```

## b. Generalized Boosted Model
```{r, ECHO=FALSE}
library(e1071)
gbm_Model <- train(classe ~ ., data = train_Train, method = "gbm", trControl = trainControl(method = "repeatedcv", number = 3, repeats = 1), verbose = FALSE)
gbm_Model
```

```{r, ECHO=TRUE}
gbm_Predict <- predict(gbm_Model, train_Validate)
confusionMatrix(gbm_Predict, train_Validate$classe)
```

## c. Random Forest Model
```{r, ECHO=FALSE}
library(randomForest)
rf_Model <- randomForest(classe ~ ., data = train_Train, method = "rf", trControl=trainControl(method = "cv", number = 4), prox = TRUE)
rf_Model
```

```{r, ECHO=TRUE}
rf_Predict <- predict(rf_Model, train_Validate)
confusionMatrix(rf_Predict, train_Validate$classe)
```


# 4) Final Model Prediction
## Apply Random Forest Model to Test dataset from the Quiz since it had a higher accuracy
```{r, ECHO=TRUE}
rf_Predict_Test <- predict(rf_Model, test)
rf_Predict_Test
```
