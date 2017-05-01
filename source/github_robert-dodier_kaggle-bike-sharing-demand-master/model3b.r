source ("my.functions.r")

train.train <- read.table ("train.train.csv", sep=",", row.names=NULL, header=T)
train.test <- read.table ("train.test.csv", sep=",", row.names=NULL, header=T)

# model 3b: estimate = mean of log(target + 1) by month, hour of day, day type
# model casual and registered separately
# should be almost equivalent to model 3 given table lookup

train.train.1 <- as.vector (train.train[, 1])
train.train.timestamp <- strptime (train.train.1, "%Y-%m-%d %H:%M:%S")

train.train.year.month.combo <- year.month.combo (train.train.timestamp)

combo.max <- max (train.train.year.month.combo)
combo.n.casual <- array (dim=c(combo.max, 24, 2), data=0)
combo.n.registered <- array (dim=c(combo.max, 24, 2), data=0)
combo.log.plus1.total.casual <- array (dim=c(combo.max, 24, 2), data=0)
combo.log.plus1.total.registered <- array (dim=c(combo.max, 24, 2), data=0)
train.train.1 <- as.vector (train.train[, 1])
train.train.timestamp <- strptime (train.train.1, "%Y-%m-%d %H:%M:%S")

for (i in 1 : length (train.train.1)) {
  j <- train.train.year.month.combo[[i]]
  k <- train.train.timestamp$hour[[i]] + 1
  l <- train.train[[i, "workingday"]] + 1
  combo.n.casual[j, k, l] <- combo.n.casual[j, k, l] + 1
  combo.n.registered[j, k, l] <- combo.n.registered[j, k, l] + 1
  # DOESN'T SEEM QUITE RIGHT TO AVERAGE LOG(CASUAL + 1) AND LOG(REGISTERED + 1) !!
  # SINCE COUNT = CASUAL + REGISTERED, NOT COUNT = CASUAL * REGISTERED !!
  combo.log.plus1.total.casual[j, k, l] <- combo.log.plus1.total.casual[j, k, l] + log (train.train$casual[[i]] + 1)
  combo.log.plus1.total.registered[j, k, l] <- combo.log.plus1.total.registered[j, k, l] + log (train.train$registered[[i]] + 1)
}

combo.count.log.plus1.estimate.casual <- combo.log.plus1.total.casual / combo.n.casual
combo.count.log.plus1.estimate.registered <- combo.log.plus1.total.registered / combo.n.registered
# if no data for a bin, let estimate = global mean-log
# could assign value for j, k combo instead !!
combo.count.log.plus1.estimate.casual [combo.n.casual == 0] <- mean (log (train.train$casual + 1))
combo.count.log.plus1.estimate.registered [combo.n.registered == 0] <- mean (log (train.train$registered + 1))

in.sample.estimate <- vector (mode="numeric", length=length(train.train.1))
in.sample.estimate.casual <- vector (mode="numeric", length=length(train.train.1))
in.sample.estimate.registered <- vector (mode="numeric", length=length(train.train.1))

for (i in 1 : length (in.sample.estimate)) {
  j <- train.train.year.month.combo[[i]]
  k <- train.train.timestamp$hour[[i]] + 1
  l <- train.train[[i, "workingday"]] + 1
  in.sample.estimate.casual[[i]] <- exp (combo.count.log.plus1.estimate.casual[j, k, l]) - 1
  in.sample.estimate.registered[[i]] <- exp (combo.count.log.plus1.estimate.registered[j, k, l]) - 1
  in.sample.estimate[[i]] <- in.sample.estimate.casual[[i]] + in.sample.estimate.registered[[i]]
}
  
rmsle (train.train$casual, in.sample.estimate.casual)
rmsle (train.train$registered, in.sample.estimate.registered)
rmsle (train.train$count, in.sample.estimate)

train.test.1 <- as.vector (train.test[, 1])
train.test.timestamp <- strptime (train.test.1, "%Y-%m-%d %H:%M:%S")
train.test.year.month.combo <- year.month.combo (train.test.timestamp)

out.of.sample.estimate <- vector (mode="numeric", length=length(train.test.1))
out.of.sample.estimate.casual <- vector (mode="numeric", length=length(train.test.1))
out.of.sample.estimate.registered <- vector (mode="numeric", length=length(train.test.1))

for (i in 1 : length (out.of.sample.estimate)) {
  j <- train.test.year.month.combo[[i]]
  k <- train.test.timestamp$hour[[i]] + 1
  l <- train.test[[i, "workingday"]] + 1
  out.of.sample.estimate.casual[[i]] <- exp (combo.count.log.plus1.estimate.casual[j, k, l]) - 1
  out.of.sample.estimate.registered[[i]] <- exp (combo.count.log.plus1.estimate.registered[j, k, l]) - 1
  out.of.sample.estimate[[i]] <- out.of.sample.estimate.casual[[i]] + out.of.sample.estimate.registered[[i]]
}

rmsle (train.test$casual, out.of.sample.estimate.casual)
rmsle (train.test$registered, out.of.sample.estimate.registered)
rmsle (train.test$count, out.of.sample.estimate)
