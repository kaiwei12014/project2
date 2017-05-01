source ("my.functions.r")

train <- read.table ("train.csv", sep=",", row.names=NULL, header=T)
test <- read.table ("test.csv", sep=",", row.names=NULL, header=T)

# model 3b: estimate = mean of log(target + 1) by month, hour of day, day type
# model casual and registered separately
# should be almost equivalent to model 3 given table lookup

train.1 <- as.vector (train[, 1])
train.timestamp <- strptime (train.1, "%Y-%m-%d %H:%M:%S")

train.year.month.combo <- year.month.combo (train.timestamp)

combo.max <- max (train.year.month.combo)
combo.n.casual <- array (dim=c(combo.max, 24, 2), data=0)
combo.n.registered <- array (dim=c(combo.max, 24, 2), data=0)
combo.log.plus1.total.casual <- array (dim=c(combo.max, 24, 2), data=0)
combo.log.plus1.total.registered <- array (dim=c(combo.max, 24, 2), data=0)

for (i in 1 : length (train.1)) {
  j <- train.year.month.combo[[i]]
  k <- train.timestamp$hour[[i]] + 1
  l <- train[[i, "workingday"]] + 1
  combo.n.casual[j, k, l] <- combo.n.casual[j, k, l] + 1
  combo.n.registered[j, k, l] <- combo.n.registered[j, k, l] + 1
  # DOESN'T SEEM QUITE RIGHT TO AVERAGE LOG(CASUAL + 1) AND LOG(REGISTERED + 1) !!
  # SINCE COUNT = CASUAL + REGISTERED, NOT COUNT = CASUAL * REGISTERED !!
  combo.log.plus1.total.casual[j, k, l] <- combo.log.plus1.total.casual[j, k, l] + log (train$casual[[i]] + 1)
  combo.log.plus1.total.registered[j, k, l] <- combo.log.plus1.total.registered[j, k, l] + log (train$registered[[i]] + 1)
}

combo.count.log.plus1.estimate.casual <- combo.log.plus1.total.casual / combo.n.casual
combo.count.log.plus1.estimate.registered <- combo.log.plus1.total.registered / combo.n.registered
# if no data for a bin, let estimate = global mean-log
# could assign value for j, k combo instead !!
combo.count.log.plus1.estimate.casual [combo.n.casual == 0] <- mean (log (train$casual + 1))
combo.count.log.plus1.estimate.registered [combo.n.registered == 0] <- mean (log (train$registered + 1))

in.sample.estimate <- vector (mode="numeric", length=length(train.1))
in.sample.estimate.casual <- vector (mode="numeric", length=length(train.1))
in.sample.estimate.registered <- vector (mode="numeric", length=length(train.1))

for (i in 1 : length (in.sample.estimate)) {
  j <- train.year.month.combo[[i]]
  k <- train.timestamp$hour[[i]] + 1
  l <- train[[i, "workingday"]] + 1
  in.sample.estimate.casual[[i]] <- exp (combo.count.log.plus1.estimate.casual[j, k, l]) - 1
  in.sample.estimate.registered[[i]] <- exp (combo.count.log.plus1.estimate.registered[j, k, l]) - 1
  in.sample.estimate[[i]] <- in.sample.estimate.casual[[i]] + in.sample.estimate.registered[[i]]
}
  
rmsle (train$casual, in.sample.estimate.casual)
rmsle (train$registered, in.sample.estimate.registered)
rmsle (train$count, in.sample.estimate)

test.1 <- as.vector (test[, 1])
test.timestamp <- strptime (test.1, "%Y-%m-%d %H:%M:%S")
test.year.month.combo <- year.month.combo (test.timestamp)

out.of.sample.estimate <- vector (mode="numeric", length=length(test.1))
out.of.sample.estimate.casual <- vector (mode="numeric", length=length(test.1))
out.of.sample.estimate.registered <- vector (mode="numeric", length=length(test.1))

for (i in 1 : length (out.of.sample.estimate)) {
  j <- test.year.month.combo[[i]]
  k <- test.timestamp$hour[[i]] + 1
  l <- test[[i, "workingday"]] + 1
  out.of.sample.estimate.casual[[i]] <- exp (combo.count.log.plus1.estimate.casual[j, k, l]) - 1
  out.of.sample.estimate.registered[[i]] <- exp (combo.count.log.plus1.estimate.registered[j, k, l]) - 1
  out.of.sample.estimate[[i]] <- out.of.sample.estimate.casual[[i]] + out.of.sample.estimate.registered[[i]]
}

test.predict <- data.frame (datetime=test$datetime, count=out.of.sample.estimate)

write.table (test.predict, "test.predict.model3b.csv", sep=",", quote=F, row.names=F)
