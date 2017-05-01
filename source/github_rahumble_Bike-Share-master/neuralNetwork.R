print("Running neural network models.")

rmsleFn = function(x, y) {
  sum(log(x+1)-log(y+1))^2
}

#trainmat = cbind(as.data.frame(model.matrix(dateTrain.lm)), count=train$count)
trainmat <- model.matrix(dateTrain.lm)
trainmat <- as.data.frame(trainmat)
trainmat <- cbind(trainmat,count=dateTrain$count)
formula = as.formula(paste("count", paste(colnames(trainmat)[-c(1,ncol(trainmat))], collapse=" + "), sep=" ~ "))

print("Fitting with train.")
train.nn = neuralnet(formula, data=trainmat,hidden=c(7,8,9,8,7),threshold=.04,stepmax=1e+06,learningrate=0.001,algorithm="rprop+",lifesign="full",likelihood=T)

pred = compute(train.nn, trainmat[,-c(1,ncol(trainmat))])$net.result
((1/10886)*sum(log(pred+1)-log(train$count+1))^2)^0.5

neuralnet(formula,data=trainmat,hidden=c(7,8,9,8,7),threshold=.04,stepmax=1e+05,algorithm="rprop+",lifesign="full",likelihood=T)



neuralnet(formula,data=trainmat,hidden=c(7,8,9,8,7),threshold=.04,stepmax=1e+05,algorithm="rprop+",lifesign="full",err.fct=rmsleFn)


#train.rf
print(rmsle.lm(train.rf, train)) #34.03