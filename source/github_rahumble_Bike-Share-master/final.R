#rmsle punishes predictions lower than the actual value more
#predict too high on purpose?

#what can we do with casual and registered?
#  especially since we don't have that information in the test set

#model casual and registered seperately??

#further things:
#  the test portion is every month past the 19th; it would be interesting to
#  do the models with just the data that had preceeded those dates, since that is what we actually
#  would have known
# more gbm optimization?
# see note on rmsle
# create lags on weather? any interactions really?

setwd("C:/Users/Ryan/Desktop/Junior Year/STAT 365/Final Project")

source("setup.R")

#source("EDA.R")

source("linearMods.R")

source("timeSeries.R")

source("generalizedBoosted.R")

source("randomForest.R")

generateFile = function(pred, scale=F) {
  pred[pred<0] = 0
  if(scale) {
    pred = pred^2 #reverse scale transformation
  }
  output = data.frame(datetime=test[,1], count=pred)
  write.csv(output, file="results.csv", quote=F, row.names=F)
}

#try predictions with various models
generateFile(predict(train.lm, newdata=test))
generateFile(predict(dateTrain.lm, newdata=dateTest))
generateFile(predict(scaleTrain.lm, newdata=test), scale=T) #reverse scale transformation
generateFile(predict(scaleDateTrain.lm, newdata=dateTest), scale=T) #reverse scale transformation

test.xreg = model.matrix(~., data=test)[,-1] #removes intercept
dateTest.xreg = model.matrix(~., data=dateTest)[,-1] #removes intercept
generateFile(predict(train.arima, newxreg=test.xreg)$pred)
generateFile(predict(dateTrain.arima, newxreg=dateTest.xreg)$pred)
generateFile(predict(scaleTrain.arima, newxreg=test.xreg)$pred, scale=T)
generateFile(predict(scaleDateTrain.arima, newxreg=dateTest.xreg)$pred, scale=T)

generateFile(predict(dateTrain.gbm, dateTest, gbm.perf(dateTrain.gbm, method="cv"), type="response"))
generateFile(predict(scaleTrain.gbm, test[,-1], gbm.perf(scaleTrain.gbm, method="cv"), type="response"), scale=T)
generateFile(predict(scaleDateTrain.gbm, dateTest, gbm.perf(scaleDateTrain.gbm, method="cv"), type="response"), scale=T)
generateFile(predict(scaleFullTrain.gbm, fullTest, gbm.perf(scaleFullTrain.gbm, method="cv"), type="response"), scale=T)

generateFile(predict(train.rf, newdata=test))
generateFile(predict(dateTrain.rf, newdata=dateTest))
generateFile(predict(scaleTrain.rf, newdata=test), scale=T) #reverse scale transformation
generateFile(predict(scaleDateTrain.rf, newdata=dateTest), scale=T) #reverse scale transformation
generateFile(predict(scaleFullTrain.rf, newdata=fullTest), scale=T) #reverse scale transformation

#the split models
generateSplit = function(pred1, pred, factor=1, logScale=F) {
  pred1[pred1<0] = 0
  pred2[pred2<0] = 0
  if(!logScale) {
    pred1 = pred1^3
    pred2 = pred2^3
  }
  else {
    pred1 = exp(pred1)-1
    pred2 = exp(pred2)-1
  }
  generateFile(factor*(pred1+pred2))
}
pred1 = predict(casualScaleDateTrain.lm, newdata=dateTest)
pred2 = predict(regScaleDateTrain.lm, newdata=dateTest)
generateSplit(pred1, pred2)

pred1 = predict(casualLogScaleFullTrain.lm, newdata=fullTest)
pred2 = predict(regLogScaleFullTrain.lm, newdata=fullTest)
generateSplit(pred1, pred2, logScale=T)

pred1 = predict(casualScaleDateTrain.arima, newxreg=dateTest.xreg)$pred
pred2 = predict(regScaleDateTrain.arima, newxreg=dateTest.xreg)$pred
generateSplit(pred1,pred2)

pred1 = predict(casualScaleDateTrain.glmnet, newx=dateTest.xreg, s="lambda.min")[,1]
pred2 = predict(regScaleDateTrain.glmnet, newx=dateTest.xreg, s="lambda.min")[,1]
generateSplit(pred1,pred2)

pred1 = predict(casualScaleDateTrain.gbm, dateTest, gbm.perf(casualScaleDateTrain.gbm, method="cv"), type="response")
pred2 = predict(regScaleDateTrain.gbm, dateTest, gbm.perf(regScaleDateTrain.gbm, method="cv"), type="response")
generateSplit(pred1,pred2)
#generateSplit(pred1,pred2,factor)

pred1 = predict(casualScaleFullTrain.gbm, fullTest, gbm.perf(casualScaleFullTrain.gbm, method="cv"), type="response")
pred2 = predict(regScaleFullTrain.gbm, fullTest, gbm.perf(regScaleFullTrain.gbm, method="cv"), type="response")
generateSplit(pred1,pred2)

pred1 = predict(casualScaleFullTrain.gbm, fullTest, gbm.perf(casualScaleFullTrain.gbm, method="cv"), type="response")
pred2 = predict(regScaleFullTrain.gbm, fullTest, gbm.perf(regScaleFullTrain.gbm, method="cv"), type="response")
generateSplit(pred1,pred2)

pred1 = predict(casualLogScaleFullTrain.gbm, fullTest, gbm.perf(casualLogScaleFullTrain.gbm, method="cv"), type="response")
pred2 = predict(regLogScaleFullTrain.gbm, fullTest, gbm.perf(regLogScaleFullTrain.gbm, method="cv"), type="response")
generateSplit(pred1,pred2,logScale=T)

pred1 = predict(casualScaleDateTrain.rf, newdata=dateTest)
pred2 = predict(regScaleDateTrain.rf, newdata=dateTest)
generateSplit(pred1, pred2)

pred1 = predict(casualScaleFullTrain.rf, newdata=fullTest)
pred2 = predict(regScaleFullTrain.rf, newdata=fullTest)
generateSplit(pred1, pred2)

pred1 = predict(casualLogScaleFullTrain.rf, newdata=fullTest)
pred2 = predict(regLogScaleFullTrain.rf, newdata=fullTest)
generateSplit(pred1, pred2, logScale=T)

pred1a[pred1a<0] = 0
pred2a[pred2a<0] = 0
pred1b[pred1b<0] = 0
pred2b[pred2b<0] = 0
pred1a = exp(pred1a)-1
pred2a = exp(pred2a)-1
pred1b = exp(pred1b)-1
pred2b = exp(pred2b)-1
generateFile(0.5*(pred1a+pred1b+pred2a+pred2b))

#submissions
1.48807 #used train.lm
1.15617 #used dateTrain.lm
1.25071 #used scaleTrain.lm
0.78259 #used scaleDateTrain.lm

1.35491 #used train.arima
1.20786 #used dateTrain.arima
1.84322 #used scaleTrain.arima
0.79629 #used scaleDateTrain.arima

0.74255 #used dateTrain.gbm
1.27811 #used scaleTrain.gbm
0.52578 #used scaleDateTrain.gbm
0.44749 #used scaleFullTrain.gbm

1.36511 #used train.rf (500 trees)
0.49532 #used scaleDateTrain.rf (500 trees)
0.49803 #used scaleDateTrain.rf (200 trees)
0.44590 #used scaleFullTrain.rf (200 trees)

0.68057 #used split scaleDateTrain.lm
0.69770 #used split scaleDateTrain.arima
0.67657 #used split scaleDateTrain.glmnet
0.45081 #used split scaleDateTrain.gbm
0.43609 #used split scaleFullTrain.gbm
!0.41930 #used split scaleLogFullTrain.gbm
0.46483 #used split scaleDateTrain.rf (500 trees, 5 mtry)
0.46083 #used split scaleDateTrain.rf (200 trees, 10 mtry)
0.42561 #used split scaleFullTrain.rf (200 trees, 10 mtry)
0.43023 #used split scaleFullTrain.rf (300 trees, 10 mtry)
~0.41269 #used split scaleLogFullTrain.rf (300 trees, 10 mtry)

:0.40959 #used average of split scaleLogFullTrain.gbm and split scaleLogFullTrain.rf (300 trees, 10 mtry)