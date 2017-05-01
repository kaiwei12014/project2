print("Running random forest models.")

ntree = 200
mtry = 10
importance = TRUE

print("Fitting with train.")
train.rf = randomForest(count ~ ., data=train, ntree=ntree, mtry=mtry, importance=importance, do.trace=T)
#train.rf
print(rmsle.lm(train.rf, train)) #33.99

print("Fitting with dateTrain.")
dateTrain.rf = randomForest(count ~ ., data=dateTrain, ntree=ntree, mtry=mtry, importance=importance, do.trace=T)
#dateTrain.rf
print(rmsle.lm(dateTrain.rf, dateTrain)) #8.60

print("Fitting with scaleTrain.")
scaleTrain.rf = randomForest(count ~ ., data=scaleTrain, ntree=ntree, mtry=mtry, importance=importance, do.trace=T)
#scaleTrain.rf
print(rmsle.lm(scaleTrain.rf, scaleTrain)) #6.92

print("Fitting with scaleDateTrain.")
scaleDateTrain.rf = randomForest(count ~ ., data=scaleDateTrain, ntree=ntree, mtry=mtry, importance=importance, do.trace=T)
#scaleDateTrain.rf
print(rmsle.lm(scaleDateTrain.rf, scaleDateTrain)) #1.04

print("Fitting with scaleFullTrain.")
scaleFullTrain.rf = randomForest(count ~ ., data=scaleFullTrain, ntree=ntree, mtry=mtry, importance=importance, do.trace=T)
#scaleFullTrain.rf
print(rmsle.lm(scaleFullTrain.rf, scaleFullTrain)) #1.06

print("Fitting with seperated scaleDateTrain.")
casualScaleDateTrain.rf = randomForest(casual ~ ., data=casualScaleDateTrain, ntree=ntree, mtry=mtry, importance=importance, do.trace=T)
regScaleDateTrain.rf = randomForest(registered ~ ., data=regScaleDateTrain, ntree=ntree, mtry=mtry, importance=importance, do.trace=T)
n = nrow(train)
actual = train[,'count']
pred1 = predict(casualScaleDateTrain.rf, newdata=casualScaleDateTrain)
pred2 = predict(regScaleDateTrain.rf, newdata=regScaleDateTrain)  
# eliminate negative predictions
pred1[pred1<0] = 0
pred2[pred2<0] = 0
pred1 = pred1^3
pred2 = pred2^3
((1/n)*sum(log((pred1+pred2)+1)-log(actual+1))^2)^0.5 #0.27

print("Fitting with seperated scaleFullTrain.")
casualScaleFullTrain.rf = randomForest(casual ~ ., data=casualScaleFullTrain, ntree=ntree, mtry=mtry, importance=importance, do.trace=T)
regScaleFullTrain.rf = randomForest(registered ~ ., data=regScaleFullTrain, ntree=ntree, mtry=mtry, importance=importance, do.trace=T)
n = nrow(train)
actual = train[,'count']
pred1 = predict(casualScaleFullTrain.rf, newdata=casualScaleFullTrain)
pred2 = predict(regScaleFullTrain.rf, newdata=regScaleFullTrain)  
# eliminate negative predictions
pred1[pred1<0] = 0
pred2[pred2<0] = 0
pred1 = pred1^3
pred2 = pred2^3
((1/n)*sum(log((pred1+pred2)+1)-log(actual+1))^2)^0.5 #0.37

print("Fitting with seperated logScaleFullTrain.")
casualLogScaleFullTrain.rf = randomForest(casual ~ ., data=casualLogScaleFullTrain, ntree=ntree, mtry=mtry, importance=importance, do.trace=T)
regLogScaleFullTrain.rf = randomForest(registered ~ ., data=regLogScaleFullTrain, ntree=ntree, mtry=mtry, importance=importance, do.trace=T)
casualLogScaleFullTrain.rf
varImpPlot(casualLogScaleFullTrain.rf)
regLogScaleFullTrain.rf
varImpPlot(regLogScaleFullTrain.rf)

varUsed(casualLogScaleFullTrain.rf)

n = nrow(train)
actual = train[,'count']
pred1 = predict(casualLogScaleFullTrain.rf, newdata=casualLogScaleFullTrain)
pred2 = predict(regLogScaleFullTrain.rf, newdata=regLogScaleFullTrain)  
# eliminate negative predictions
pred1[pred1<0] = 0
pred2[pred2<0] = 0
pred1 = exp(pred1)-1
pred2 = exp(pred2)-1
((1/n)*sum(log((pred1+pred2)+1)-log(actual+1))^2)^0.5 #0.92

par(mfrow=c(1,1))
plot(train$datetime, actual, ylab="Hourly Riders", xlab="Year", main="Actual vs. Predicted Ridership Levels")
points(train$datetime, pred1+pred2, col="red")
legend("topleft", c("Actual","Predicted"), col=c("black","red"), pch=c(1,1))
