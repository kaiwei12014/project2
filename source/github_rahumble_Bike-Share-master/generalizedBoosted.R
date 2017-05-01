print("Running generalized boosted regression models.")

rmsle.gbm = function(model, data, multiplier=1) {
  bestIter = gbm.perf(model, method="cv")
  print(bestIter)
  print(model$cv.error[bestIter])
  #pretty.gbm.tree(train.gbm, train.bestIter)
  #summary(train.gbm, n.trees=train.bestIter)
  
  pred = predict(model, data, bestIter, type="response")
  pred[pred<0] = 0
  n = nrow(train)
  
  ((1/n)*sum(log(multiplier*pred+1)-log(data$count+1))^2)^0.5
}

#try generalized boosted regression
train.gbm = gbm(count ~ .
                ,data=train[,-1], 
                ,var.monotone=NULL # which vars go up or down with target
                ,distribution="gaussian"
                ,n.trees=15000
                ,shrinkage=0.01
                ,interaction.depth=4
                ,bag.fraction = 0.5
                ,train.fraction = 1
                ,n.minobsinnode = 10
                ,cv.folds = 10
                ,keep.data=TRUE
                ,verbose=TRUE)
rmsle.gbm(train.gbm, train[,-1])

dateTrain.gbm = gbm(count ~ .
                ,data=dateTrain, 
                ,var.monotone=NULL # which vars go up or down with target
                ,distribution="gaussian"
                ,n.trees=25000
                ,shrinkage=0.01
                ,interaction.depth=4
                ,bag.fraction = 0.5
                ,train.fraction = 1
                ,n.minobsinnode = 10
                ,cv.folds = 10
                ,keep.data=TRUE
                ,verbose=TRUE)
rmsle.gbm(dateTrain.gbm, dateTrain)

scaleTrain.gbm = gbm(count ~ .
                    ,data=scaleTrain[,-1], 
                    ,var.monotone=NULL # which vars go up or down with target
                    ,distribution="gaussian"
                    ,n.trees=15000
                    ,shrinkage=0.01
                    ,interaction.depth=4
                    ,bag.fraction = 0.5
                    ,train.fraction = 1
                    ,n.minobsinnode = 10
                    ,cv.folds = 10
                    ,keep.data=TRUE
                    ,verbose=TRUE)
rmsle.gbm(scaleTrain.gbm, scaleTrain[,-1])

scaleDateTrain.gbm = gbm(count ~ .
                     ,data=scaleDateTrain, 
                     ,var.monotone=NULL # which vars go up or down with target
                     ,distribution="gaussian"
                     ,n.trees=20000
                     ,shrinkage=0.01
                     ,interaction.depth=4
                     ,bag.fraction = 0.5
                     ,train.fraction = 1
                     ,n.minobsinnode = 10
                     ,cv.folds = 10
                     ,keep.data=TRUE
                     ,verbose=TRUE)
rmsle.gbm(scaleDateTrain.gbm, scaleDateTrain)

scaleFullTrain.gbm = gbm(count ~ .
                         ,data=scaleFullTrain, 
                         ,var.monotone=NULL # which vars go up or down with target
                         ,distribution="gaussian"
                         ,n.trees=20000
                         ,shrinkage=0.01
                         ,interaction.depth=4
                         ,bag.fraction = 0.5
                         ,train.fraction = 1
                         ,n.minobsinnode = 10
                         ,cv.folds = 10
                         ,keep.data=TRUE
                         ,verbose=TRUE)
rmsle.gbm(scaleFullTrain.gbm, scaleFullTrain)

casualScaleDateTrain.gbm = gbm(casual ~ .
                               ,data=casualScaleDateTrain, 
                               ,var.monotone=NULL # which vars go up or down with target
                               ,distribution="gaussian"
                               ,n.trees=20000
                               ,shrinkage=0.01
                               ,interaction.depth=4
                               ,bag.fraction = 0.5
                               ,train.fraction = 1
                               ,n.minobsinnode = 10
                               ,cv.folds = 10
                               ,keep.data=TRUE
                               ,verbose=TRUE)
bestIter = gbm.perf(casualScaleDateTrain.gbm, method="cv")
print(bestIter)
pred1 = predict(casualScaleDateTrain.gbm, casualScaleDateTrain, bestIter, type="response")

regScaleDateTrain.gbm = gbm(registered ~ .
                            ,data=regScaleDateTrain, 
                            ,var.monotone=NULL # which vars go up or down with target
                            ,distribution="gaussian"
                            ,n.trees=20000
                            ,shrinkage=0.01
                            ,interaction.depth=4
                            ,bag.fraction = 0.5
                            ,train.fraction = 1
                            ,n.minobsinnode = 10
                            ,cv.folds = 10
                            ,keep.data=TRUE
                            ,verbose=TRUE)
bestIter = gbm.perf(regScaleDateTrain.gbm, method="cv")
print(bestIter)
pred2 = predict(regScaleDateTrain.gbm, regScaleDateTrain, bestIter, type="response")
n = nrow(train)
actual = train[,'count']
# eliminate negative predictions
pred1[pred1<0] = 0
pred2[pred2<0] = 0
pred1 = pred1^3
pred2 = pred2^3

par(mfrow=c(1,1))
plot(train$datetime, actual, ylab="Hourly Riders", xlab="Year", main="Actual vs. Predicted Ridership Levels")
points(train$datetime, pred1+pred2, col="red")
legend("topleft", c("Actual","Predicted"), col=c("black","red"), pch=c(1,1))

((1/n)*sum(log((pred1+pred2)+1)-log(actual+1))^2)^0.5 #1.2278
  
loc = function(multiplier) { 
  ((1/n)*sum(log(multiplier*(pred1+pred2)+1)-log(actual+1))^2)^0.5
}

plot(x=NULL, y=NULL, xlim=c(0,5), ylim=c(0, loc(5)))
for(i in seq(0, 5, 0.01)) {
  points(i, loc(i))
}
opt = optimize(loc, lower=0, upper=10)
abline(v=opt$minimum)
factor = opt$minimum

casualScaleFullTrain.gbm = gbm(casual ~ .
                               ,data=casualScaleFullTrain, 
                               ,var.monotone=NULL # which vars go up or down with target
                               ,distribution="gaussian"
                               ,n.trees=20000
                               ,shrinkage=0.01
                               ,interaction.depth=4
                               ,bag.fraction = 0.5
                               ,train.fraction = 1
                               ,n.minobsinnode = 10
                               ,cv.folds = 10
                               ,keep.data=TRUE
                               ,verbose=TRUE)
bestIter = gbm.perf(casualScaleFullTrain.gbm, method="cv")
print(bestIter)
pred1 = predict(casualScaleFullTrain.gbm, casualScaleFullTrain, bestIter, type="response")

regScaleFullTrain.gbm = gbm(registered ~ .
                            ,data=regScaleFullTrain, 
                            ,var.monotone=NULL # which vars go up or down with target
                            ,distribution="gaussian"
                            ,n.trees=20000
                            ,shrinkage=0.01
                            ,interaction.depth=4
                            ,bag.fraction = 0.5
                            ,train.fraction = 1
                            ,n.minobsinnode = 10
                            ,cv.folds = 10
                            ,keep.data=TRUE
                            ,verbose=TRUE)
bestIter = gbm.perf(regScaleFullTrain.gbm, method="cv")
print(bestIter)
pred2 = predict(regScaleFullTrain.gbm, regScaleFullTrain, bestIter, type="response")
n = nrow(train)
actual = train[,'count']
# eliminate negative predictions
pred1[pred1<0] = 0
pred2[pred2<0] = 0
pred1 = pred1^3
pred2 = pred2^3
((1/n)*sum(log((pred1+pred2)+1)-log(actual+1))^2)^0.5 #1.2245

casualLogScaleFullTrain.gbm = gbm(casual ~ .
                               ,data=casualLogScaleFullTrain, 
                               ,var.monotone=NULL # which vars go up or down with target
                               ,distribution="gaussian"
                               ,n.trees=20000
                               ,shrinkage=0.01
                               ,interaction.depth=4
                               ,bag.fraction = 0.5
                               ,train.fraction = 1
                               ,n.minobsinnode = 10
                               ,cv.folds = 10
                               ,keep.data=TRUE
                               ,verbose=TRUE)
bestIter = gbm.perf(casualLogScaleFullTrain.gbm, method="cv")
print(bestIter)
pred1 = predict(casualLogScaleFullTrain.gbm, casualLogScaleFullTrain, bestIter, type="response")

regLogScaleFullTrain.gbm = gbm(registered ~ .
                            ,data=regLogScaleFullTrain, 
                            ,var.monotone=NULL # which vars go up or down with target
                            ,distribution="gaussian"
                            ,n.trees=20000
                            ,shrinkage=0.01
                            ,interaction.depth=4
                            ,bag.fraction = 0.5
                            ,train.fraction = 1
                            ,n.minobsinnode = 10
                            ,cv.folds = 10
                            ,keep.data=TRUE
                            ,verbose=TRUE)
bestIter = gbm.perf(regLogScaleFullTrain.gbm, method="cv")
print(bestIter)
pred2 = predict(regLogScaleFullTrain.gbm, regLogScaleFullTrain, bestIter, type="response")
n = nrow(train)
actual = train[,'count']
# eliminate negative predictions
pred1[pred1<0] = 0
pred2[pred2<0] = 0
pred1 = exp(pred1)-1
pred2 = exp(pred2)-1
((1/n)*sum(log((pred1+pred2)+1)-log(actual+1))^2)^0.5 #1.2245

par(mfrow=c(1,1))
plot(train$datetime, actual, ylab="Hourly Riders", xlab="Year", main="Actual vs. Predicted Ridership Levels")
points(train$datetime, pred1+pred2, col="red")
legend("topleft", c("Actual","Predicted"), col=c("black","red"), pch=c(1,1))

((1/n)*sum(log((pred1+pred2)+1)-log(actual+1))^2)^0.5 #1.2278