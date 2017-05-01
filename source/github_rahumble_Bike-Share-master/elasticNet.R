print("Running elastic net models.")

rmsle.cv.glmnet = function(model.cv.glmnet, model.lm, multiplier=1, plot=T) {
  newxreg = model.matrix(model.lm)[,-1]
  n = nrow(newxreg)
  actual = model.lm$model[,'count']
  pred = predict(model.cv.glmnet, newx=newxreg, s="lambda.min")
  # eliminate negative predictions
  pred[pred<0] = 0
  pred = multiplier*pred
  
  if(plot) {
    plot(1:n, actual)
    points(1:n, pred, col="red")
  }
  
  ((1/n)*sum(log(pred+1)-log(actual+1))^2)^0.5
}
rmsleOpt.cv.glmnet = function(model.cv.glmnet, model.lm) {
  newxreg = model.matrix(model.lm)[,-1]
  n = nrow(newxreg)
  actual = model.lm$model[,'count']
  pred = predict(model.cv.glmnet, newx=newxreg, s="lambda.min")
  # eliminate negative predictions
  pred[pred<0] = 0
  
  loc = function(multiplier) {
    ((1/n)*sum(log(multiplier*pred+1)-log(actual+1))^2)^0.5
  }
  plot(x=NULL, y=NULL, xlim=c(0,5), ylim=c(0, loc(5)))
  for(i in seq(0, 5, 0.01)) {
    points(i, loc(i))
  }
  opt = optimize(loc, lower=0, upper=10)
  abline(v=opt$minimum)
  
  print(opt$minimum)
  rmsle.cv.glmnet(model.cv.glmnet, model.lm, opt$minimum)
}

#optimize over alpha
glmnetOpt = function(model.lm) {
  xreg = model.matrix(model.lm)[,-1] #removes intercept
  actual = model.lm$model[,'count']
  
  loc = function(alpha) {
    mod = cv.glmnet(xreg, actual, alpha=alpha, nlambda=1000)
    rmsle.cv.glmnet(mod, model.lm, plot=F)
  }
  
  optimize(loc, lower=0, upper=1)$minimum
}

print("Fitting with train.")
train.alpha = glmnetOpt(train.lm)
#train.alpha
train.glmnet = cv.glmnet(train.xreg, train$count, alpha=train.alpha, nlambda=1000)
print(rmsle.cv.glmnet(train.glmnet, train.lm)) #40.455

print("Fitting with dateTrain.")
dateTrain.alpha = glmnetOpt(dateTrain.lm)
#dateTrain.alpha
dateTrain.glmnet = cv.glmnet(dateTrain.xreg, dateTrain$count, alpha=dateTrain.alpha, nlambda=1000)
print(rmsle.cv.glmnet(dateTrain.glmnet, dateTrain.lm)) #0.814

print("Fitting with scaleTrain.")
scaleTrain.alpha = glmnetOpt(scaleTrain.lm)
#scaleTrain.alpha
scaleTrain.glmnet = cv.glmnet(scaleTrain.xreg, scaleTrain$count, alpha=scaleTrain.alpha, nlambda=1000)
print(rmsle.cv.glmnet(scaleTrain.glmnet, scaleTrain.lm)) #12.307

print("Fitting with scaleDateTrain.")
scaleDateTrain.alpha = glmnetOpt(scaleDateTrain.lm)
#scaleDateTrain.alpha
scaleDateTrain.glmnet = cv.glmnet(scaleDateTrain.xreg, scaleDateTrain$count, alpha=scaleDateTrain.alpha, nlambda=1000)
print(rmsle.cv.glmnet(scaleDateTrain.glmnet, scaleDateTrain.lm)) #1.401

print("Fitting with seperated scaleDateTrain.")
actual = train[,'count']

locCas = function(alpha) {
  mod = cv.glmnet(scaleDateTrain.xreg, casual, alpha=alpha, nlambda=1000)
  rmsle.cv.glmnet(mod, scaleDateTrain.lm, plot=F)
}
locReg = function(alpha) {
  mod = cv.glmnet(scaleDateTrain.xreg, registered, alpha=alpha, nlambda=1000)
  rmsle.cv.glmnet(mod, scaleDateTrain.lm, plot=F)
}
casualScaleDateTrain.alpha = optimize(locCas, lower=0, upper=1)$minimum
regScaleDateTrain.alpha = optimize(locReg, lower=0, upper=1)$minimum

casualScaleDateTrain.glmnet = cv.glmnet(scaleDateTrain.xreg, casualScaleDateTrain$casual, alpha=casualScaleDateTrain.alpha, nlambda=1000)
regScaleDateTrain.glmnet = cv.glmnet(scaleDateTrain.xreg, regScaleDateTrain$registered, alpha=regScaleDateTrain.alpha, nlambda=1000)
n = nrow(train)
actual = train[,'count']
pred1 = predict(casualScaleDateTrain.glmnet, newx=scaleDateTrain.xreg, s="lambda.min")
pred2 = predict(regScaleDateTrain.glmnet, newx=scaleDateTrain.xreg, s="lambda.min") 
# eliminate negative predictions
pred1[pred1<0] = 0
pred2[pred2<0] = 0
pred1 = pred1^3
pred2 = pred2^3

plot(1:n, actual)
points(1:n, pred1+pred2, col="red")

((1/n)*sum(log((pred1+pred2)+1)-log(actual+1))^2)^0.5 #3.564

print("Optimal RMSLE multipliers.")
rmsleOpt.cv.glmnet(train.glmnet, train.lm)
rmsleOpt.cv.glmnet(dateTrain.glmnet, dateTrain.lm)
rmsleOpt.cv.glmnet(scaleTrain.glmnet, scaleTrain.lm)
rmsleOpt.cv.glmnet(scaleDateTrain.glmnet, scaleDateTrain.lm)