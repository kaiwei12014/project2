print("Running linear models.")

rmsle.lm = function(model, data, multiplier=1) {
  n = nrow(data)
  actual = data[,'count']
  pred = predict(model, newdata=data)  
  # eliminate negative predictions
  pred[pred<0] = 0
  pred = multiplier*pred
  
  plot(1:n, actual)
  points(1:n, pred, col="red")
  
  ((1/n)*sum(log(pred+1)-log(actual+1))^2)^0.5
}

print("Fitting with train.")
train.lm = lm(count ~ ., data=train)
#summary(train.lm) #0.3216 R^2 value
print(rmsle.lm(train.lm, train)) #39.607
#predicted values are just too regular, don't capture variability

print("Fitting with dateTrain.")
dateTrain.lm = lm(count ~ ., data=dateTrain)
#summary(dateTrain.lm) #0.6926 R^2 value
print(rmsle.lm(dateTrain.lm, dateTrain)) #0.1563
#predicted values are just too regular, don't capture variability

print("Fitting with scaleTrain.")
scaleTrain.lm = lm(count ~ ., data=scaleTrain)
#summary(scaleTrain.lm) #0.3445 R^2 value
print(rmsle.lm(scaleTrain.lm, scaleTrain)) #12.176
#R^2 is not any better but the rmsle is

print("Fitting with scaleDateTrain.")
scaleDateTrain.lm = lm(count ~ ., data=scaleDateTrain)
#summary(scaleDateTrain.lm) #0.7912 R^2 value
print(rmsle.lm(scaleDateTrain.lm, scaleDateTrain)) #1.122

scaleFullTrain.lm = lm(count ~ ., data=scaleFullTrain)

print("Fitting with seperated scaleDateTrain.")
casualScaleDateTrain.lm = lm(casual ~ ., data=casualScaleDateTrain)
regScaleDateTrain.lm = lm(registered ~ ., data=regScaleDateTrain)
n = nrow(train)
actual = train[,'count']
pred1 = predict(casualScaleDateTrain.lm, newdata=casualScaleDateTrain)
pred2 = predict(regScaleDateTrain.lm, newdata=regScaleDateTrain)  
# eliminate negative predictions
pred1[pred1<0] = 0
pred2[pred2<0] = 0
pred1 = pred1^3
pred2 = pred2^3

par(mfrow=c(1,1))
plot(train$datetime, actual, ylab="Hourly Riders", xlab="Year", main="Actual vs. Predicted Ridership Levels")
points(train$datetime, pred1+pred2, col="red")
legend("topleft", c("Actual","Predicted"), col=c("black","red"), pch=c(1,1))

((1/n)*sum(log((pred1+pred2)+1)-log(actual+1))^2)^0.5 #3.255

print("Fitting with seperated LogScaleFullTrain.")
casualLogScaleFullTrain.lm = lm(casual ~ ., data=casualLogScaleFullTrain)
regLogScaleFullTrain.lm = lm(registered ~ ., data=regLogScaleFullTrain)
n = nrow(train)
actual = train[,'count']
pred1 = predict(casualLogScaleFullTrain.lm, newdata=casualLogScaleFullTrain)
pred2 = predict(regLogScaleFullTrain.lm, newdata=regLogScaleFullTrain)  
# eliminate negative predictions
pred1[pred1<0] = 0
pred2[pred2<0] = 0
pred1 = exp(pred1)-1
pred2 = exp(pred2)-1

par(mfrow=c(1,1))
plot(train$datetime, actual, ylab="Hourly Riders", xlab="Year", main="Actual vs. Predicted Ridership Levels")
points(train$datetime, pred1+pred2, col="red")
legend("topleft", c("Actual","Predicted"), col=c("black","red"), pch=c(1,1))

((1/n)*sum(log((pred1+pred2)+1)-log(actual+1))^2)^0.5 #1.465