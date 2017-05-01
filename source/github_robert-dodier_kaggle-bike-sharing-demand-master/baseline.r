source ("my.functions.r")

train.train <- read.table ("train.train.csv", sep=",", row.names=NULL, header=T)
train.test <- read.table ("train.test.csv", sep=",", row.names=NULL, header=T)
train <- read.table ("train.csv", sep=",", row.names=NULL, header=T)

in.sample.estimate <- vector (mode="numeric", length=length(train.train$count))
out.of.sample.estimate <- vector (mode="numeric", length=length(train.test$count))

# baseline 0: mean of target

train.train.count.mean <- mean (train.train$count)

in.sample.estimate[] <- train.train.count.mean
rmsle (train.train$count, in.sample.estimate)

out.of.sample.estimate[] <- train.train.count.mean
rmsle (train.test$count, out.of.sample.estimate)

# baseline 1: mean of log(target + 1)

train.train.count.mean.log.plus1 <- mean (log (train.train$count + 1))

in.sample.estimate[] <- exp (train.train.count.mean.log.plus1) - 1
rmsle (train.train$count, in.sample.estimate)

out.of.sample.estimate[] <- exp (train.train.count.mean.log.plus1) - 1
rmsle (train.test$count, out.of.sample.estimate)

