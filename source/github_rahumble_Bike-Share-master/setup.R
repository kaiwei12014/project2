print("Running setup.\n")
library(forecast)
library(elasticnet)
library(glmnet)
library(gbm)
library(mgcv)
library(caret)
library(randomForest)

train = read.csv(file="train.csv", stringsAsFactors=F)
test = read.csv(file="test.csv", stringsAsFactors=F)

casual = train$casual
registered = train$registered
train = train[,-c(10,11)]

train$datetime = as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M:%S")
train$season = factor(train$season, labels=c("Spring", "Summer", "Fall", "Winter"))
train$holiday = factor(train$holiday, labels=c("Non-holiday", "Holiday"))
train$workingday = factor(train$workingday, labels=c("Non-workingday", "Workingday"))
train$weather = factor(train$weather, labels=c("Clear", "Cloudy", "RainSnow", "4"))
#change the 1 data point for weather = 4 to a 3
train$weather[which(train$weather==4)] = "RainSnow"
train$weather = factor(train$weather)

#breaking time down into components
dateTrain = train
dateTrain$hour = as.factor(as.numeric(format(dateTrain$datetime, "%H")))
dateTrain$day = as.factor(weekdays(dateTrain$datetime))
dateTrain$month = as.factor(as.numeric(format(dateTrain$datetime, "%m")))
dateTrain$year = as.factor(as.numeric(format(dateTrain$datetime, "%Y")))
scaleFullTrain = dateTrain[,-1]
dateTrain = dateTrain[,-c(1,2,4)]

#what if we did a scale transformation on count
scaleTrain = train
scaleTrain$count = scaleTrain$count^(1/2)

#combining date and scale
scaleDateTrain = dateTrain
scaleDateTrain$count = scaleDateTrain$count^(1/2)

#adding timeday and sunday, and scale
hour = as.numeric(scaleFullTrain$hour)
#4am-10am = 1, 11am-3pm = 2, 4pm-9pm = 3, 10pm-3am = 4
scaleFullTrain$timeday = "Morning"
scaleFullTrain$timeday[(hour < 10) & (hour > 3)] = "Midday"
scaleFullTrain$timeday[(hour < 16) & (hour >= 10)] = "Evening"
scaleFullTrain$timeday[(hour < 22) & (hour >= 16)] = "Overnight"
scaleFullTrain$timeday = as.factor(scaleFullTrain$timeday)
scaleFullTrain$sunday = "0"
scaleFullTrain$sunday[scaleFullTrain$day == "Sunday"] = "1"
scaleFullTrain$sunday = as.factor(scaleFullTrain$sunday)
scaleFullTrain$count = scaleFullTrain$count^(1/2)

#combining date seperation, scaling, and regressing on casual and registered seperately
casualScaleDateTrain = scaleDateTrain[,-7]
casualScaleDateTrain$casual = casual^(1/3)
regScaleDateTrain = scaleDateTrain[,-7]
regScaleDateTrain$registered = registered^(1/3)

#combining everything
casualScaleFullTrain = scaleFullTrain[,-9]
casualScaleFullTrain$casual = casual^(1/3)
regScaleFullTrain = scaleFullTrain[,-9]
regScaleFullTrain$registered = registered^(1/3)

#different scaling (one in line with error function)
casualLogScaleFullTrain = scaleFullTrain[,-9]
casualLogScaleFullTrain$casual = log(casual+1)
regLogScaleFullTrain = scaleFullTrain[,-9]
regLogScaleFullTrain$registered = log(registered+1)

#recreate the test versions
test$datetime = as.POSIXct(test$datetime, format="%Y-%m-%d %H:%M:%S")
test$season = factor(test$season, labels=c("Spring", "Summer", "Fall", "Winter"))
test$holiday = factor(test$holiday, labels=c("Non-holiday", "Holiday"))
test$workingday = factor(test$workingday, labels=c("Non-workingday", "Workingday"))
test$weather = factor(test$weather, labels=c("Clear", "Cloudy", "RainSnow", "4"))
#change the 1 data point for weather = 4 to a 3
test$weather[which(test$weather==4)] = "RainSnow"
test$weather = factor(test$weather)

dateTest = test
dateTest$hour = as.factor(as.numeric(format(dateTest$datetime, "%H")))
dateTest$day = as.factor(weekdays(dateTest$datetime))
dateTest$month = as.factor(as.numeric(format(dateTest$datetime, "%m")))
dateTest$year = as.factor(as.numeric(format(dateTest$datetime, "%Y")))
fullTest = dateTest[,-1]
dateTest = dateTest[,-c(1,2,4)]

hour = as.numeric(fullTest$hour)
fullTest$timeday = "Morning"
fullTest$timeday[(hour < 10) & (hour > 3)] = "Midday"
fullTest$timeday[(hour < 16) & (hour > 9)] = "Evening"
fullTest$timeday[(hour < 22) & (hour > 15)] = "Overnight"
fullTest$timeday = as.factor(fullTest$timeday)
fullTest$sunday = "0"
fullTest$sunday[fullTest$day == "Sunday"] = "1"
fullTest$sunday = as.factor(fullTest$sunday)