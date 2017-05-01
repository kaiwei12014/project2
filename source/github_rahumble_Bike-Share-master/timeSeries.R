print("Running time series.")

rmsle.arima = function(model.arima, model.lm, multiplier=1) {
  newxreg = model.matrix(model.lm)[,-1]
  n = nrow(newxreg)
  actual = model.lm$model[,'count']
  pred = predict(model.arima, newxreg=newxreg)  
  # eliminate negative predictions
  pred$pred[pred$pred<0] = 0
  pred$pred = multiplier*pred$pred
  
  plot(1:n, actual)
  points(1:n, pred$pred, col="red")
  
  ((1/n)*sum(log(pred$pred+1)-log(actual+1))^2)^0.5
}
rmsleOpt.arima = function(model.arima, model.lm) {
  newxreg = model.matrix(model.lm)[,-1]
  n = nrow(newxreg)
  actual = model.lm$model[,'count']
  pred = predict(model.arima, newxreg=newxreg)  
  # eliminate negative predictions
  pred$pred[pred$pred<0] = 0
  
  loc = function(multiplier) {
    ((1/n)*sum(log(multiplier*pred$pred+1)-log(actual+1))^2)^0.5
  }
  plot(x=NULL, y=NULL, xlim=c(0,5), ylim=c(0, loc(5)))
  for(i in seq(0, 5, 0.01)) {
    points(i, loc(i))
  }
  opt = optimize(loc, lower=0, upper=10)
  abline(v=opt$minimum)
  
  print(opt$minimum)
  rmsle.arima(model.arima, model.lm, opt$minimum)
}

#trying time series analysis
print("Fitting with train.")
train.xreg = model.matrix(train.lm)[,-1] #removes intercept
train.arima = auto.arima(train$count, xreg=train.xreg)
#summary(train.arima) #has lower AIC than train.lm
print(rmsle.arima(train.arima, train.lm)) #108.74

print("Fitting with dateTrain.")
dateTrain.xreg = model.matrix(dateTrain.lm)[,-1] #removes intercept
dateTrain.arima = auto.arima(dateTrain$count, xreg=dateTrain.xreg)
#summary(dateTrain.arima) #has lower AIC than dateTrain.lm
print(rmsle.arima(dateTrain.arima, dateTrain.lm)) #71.093

print("Fitting with scaleTrain.")
scaleTrain.xreg = model.matrix(scaleTrain.lm)[,-1] #removes intercept
scaleTrain.arima = auto.arima(scaleTrain$count, xreg=scaleTrain.xreg)
#summary(scaleTrain.arima) #has lower AIC than scaleTrain
print(rmsle.arima(scaleTrain.arima, scaleTrain.lm)) #59.65

print("Fitting with scaleDateTrain.")
scaleDateTrain.xreg = model.matrix(scaleDateTrain.lm)[,-1] #removes intercept
scaleDateTrain.arima = auto.arima(scaleDateTrain$count, xreg=scaleDateTrain.xreg)
#summary(scaleDateTrain.arima) #has lower AIC than linear model
print(rmsle.arima(scaleDateTrain.arima, scaleDateTrain.lm)) #58.413

print("Fitting with seperated scaleDateTrain.")
casualScaleDateTrain.arima = auto.arima(casualScaleDateTrain$casual, xreg=scaleDateTrain.xreg)
regScaleDateTrain.arima = auto.arima(regScaleDateTrain$registered, xreg=scaleDateTrain.xreg)
n = nrow(train)
actual = train[,'count']
pred1 = predict(casualScaleDateTrain.arima, newxreg=scaleDateTrain.xreg)$pred
pred2 = predict(regScaleDateTrain.arima, newxreg=scaleDateTrain.xreg)$pred  
# eliminate negative predictions
pred1[pred1<0] = 0
pred2[pred2<0] = 0
pred1 = pred1^3
pred2 = pred2^3

plot(train$datetime, actual)
points(train$datetime, pred1+pred2, col="red")

((1/n)*sum(log((pred1+pred2)+1)-log(actual+1))^2)^0.5 #12.5446