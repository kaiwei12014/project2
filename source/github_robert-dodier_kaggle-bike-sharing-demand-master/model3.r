source ("my.functions.r")

train.train <- read.table ("train.train.csv", sep=",", row.names=NULL, header=T)
train.test <- read.table ("train.test.csv", sep=",", row.names=NULL, header=T)

# model 3: estimate = mean of log(target + 1) by month, hour of day, day type

train.train.1 <- as.vector (train.train[, 1])
train.train.timestamp <- strptime (train.train.1, "%Y-%m-%d %H:%M:%S")

train.train.year.month.combo <- year.month.combo (train.train.timestamp)

combo.max <- max (train.train.year.month.combo)
combo.n <- array (dim=c(combo.max, 24, 2), data=0)
combo.log.plus1.total <- array (dim=c(combo.max, 24, 2), data=0)
train.train.1 <- as.vector (train.train[, 1])
train.train.timestamp <- strptime (train.train.1, "%Y-%m-%d %H:%M:%S")

for (i in 1 : length (train.train.1)) {
  j <- train.train.year.month.combo[[i]]
  k <- train.train.timestamp$hour[[i]] + 1
  l <- train.train[[i, "workingday"]] + 1
  combo.n[j, k, l] <- combo.n[j, k, l] + 1
  combo.log.plus1.total[j, k, l] <- combo.log.plus1.total[j, k, l] + log (train.train$count[[i]] + 1)
}

combo.count.log.plus1.estimate <- combo.log.plus1.total / combo.n
# if no data for a bin, let estimate = global mean-log
# could assign value for j, k combo instead !!
combo.count.log.plus1.estimate [combo.n == 0] <- mean (log (train.train$count + 1))

in.sample.estimate <- vector (mode="numeric", length=length(train.train.1))

for (i in 1 : length (in.sample.estimate)) {
  j <- train.train.year.month.combo[[i]]
  k <- train.train.timestamp$hour[[i]] + 1
  l <- train.train[[i, "workingday"]] + 1
  in.sample.estimate[[i]] <- exp (combo.count.log.plus1.estimate[j, k, l]) - 1
}
  
rmsle (train.train$count, in.sample.estimate)

train.test.1 <- as.vector (train.test[, 1])
train.test.timestamp <- strptime (train.test.1, "%Y-%m-%d %H:%M:%S")
train.test.year.month.combo <- year.month.combo (train.test.timestamp)

out.of.sample.estimate <- vector (mode="numeric", length=length(train.test.1))

for (i in 1 : length (out.of.sample.estimate)) {
  j <- train.test.year.month.combo[[i]]
  k <- train.test.timestamp$hour[[i]] + 1
  l <- train.test[[i, "workingday"]] + 1
  out.of.sample.estimate[[i]] <- exp (combo.count.log.plus1.estimate[j, k, l]) - 1
}

rmsle (train.test$count, out.of.sample.estimate)

my.sle <- sle (train.test$count, out.of.sample.estimate)
summary (my.sle)

