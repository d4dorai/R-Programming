---
title: "Weight Lifting"
author: "DS"
date: "Sunday, August 24, 2014"
output: html_document
---
##The Data

The Weight Lifting Exercises dataset is meant to investigate how well an activity is performed by the wearer. See <http://groupware.les.inf.puc-rio.br/har>

Six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions: A (correct), B, C, D, E.

The main variables: With 4 sensors measurements from 3 angles were taken from the (1)arm (2)forearm (3)belt (4)dumbbell. Readings were also included from the accelerometer, gyroscope and magnetometer. Additionally, for each sensor angle the following quantities were also calculated: mean, variance,standard deviation, max, min, amplitude, kurtosis and skewness. 

## Analysis

(1) Load the data:

```{r}
dataS <- read.csv("pml-training.csv")
dataT <- read.csv("pml-testing.csv")
```

### Data Cleaning
(2) Clean the data by eliminating useless variables: columns with more than 95% missing values and columns with no information other than for indexing purposes:

```{r}
threshold <- dim(dataS)[1]*0.95
empty <- apply(dataS, 2, function(x)
        sum(is.na(x)) > threshold  || sum(x=="") > threshold)
dataS <- dataS[,!empty]

detail <- grep("timestamp|X|user_name|new_window",names(dataS))
dataS <- dataS[,-detail]

dataS$classe <- factor(dataS$classe)
```

We are now left with 53 predictor variables and 1 outcome variable!

(3) Partition the data into a training set and a cross-validation set

```{r}
set.seed(5)
library(caret)
datapart <- createDataPartition(dataS$classe, p = 0.7, list = FALSE)
data_train <- dataS[datapart, ]
data_cv <- dataS[-datapart, ]
```

###The Model

(4) Calculate a Random Forest Models as suggested by the associated paper (Velloso et. al. 2013) that comes with the data, due to the inherent noise of signal measurement.

```{r}
model_rf <- train(classe ~ ., data=data_train, method="rf")

```

(5) In-Sample Prediction (Training Set)
```{r}
predict_train <- predict(model_rf, data_train)
print(confusionMatrix(predict_train, data_train$classe))
```
The in-samplee accuracy is 100%. This gives us reason to worry about overfitting the data.

(6) Out-of-Sample Prediction (Cross Validation Set)
```{r}
predict_cv <- predict(model_rf, data_cv)
print(confusionMatrix(predict_cv, data_cv$classe))
```
As we can see the out of sample accuracy is 99.78%. Therefore the out of sample error is 0.22%. This is a remarkably accurate method!

### References

Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013
