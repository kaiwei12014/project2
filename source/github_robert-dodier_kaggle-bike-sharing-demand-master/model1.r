source ("my.functions.r")

train.train <- read.table ("train.train.csv", sep=",", row.names=NULL, header=T)
train.test <- read.table ("train.test.csv", sep=",", row.names=NULL, header=T)

# model 1: estimate = mean of log(target + 1) by months

train.train.1 <- as.vector (train.train[, 1])
train.train.timestamp <- strptime (train.train.1, "%Y-%m-%d %H:%M:%S")

train.train.year.month.combo <- year.month.combo (train.train.timestamp)

combo.max <- max (train.train.year.month.combo)
combo.n <- vector (mode="numeric", length=combo.max)
combo.log.plus1.total <- vector (mode="numeric", length=combo.max)
train.train.log.count.plus1 <- log (train.train$count + 1)

for (i in 1 : length (train.train.1)) {
  j <- train.train.year.month.combo[[i]]
  combo.n[[j]] <- combo.n[[j]] + 1
  combo.log.plus1.total[[j]] <- combo.log.plus1.total[[j]] + train.train.log.count.plus1[[i]]
}

combo.count.log.plus1.estimate <- combo.log.plus1.total / combo.n

in.sample.estimate <- vector (mode="numeric", length=length(train.train.1))

for (i in 1 : length (in.sample.estimate)) {
  j <- train.train.year.month.combo[[i]]
  in.sample.estimate[[i]] <- exp (combo.count.log.plus1.estimate[[j]]) - 1
}

rmsle (train.train$count, in.sample.estimate)

train.test.1 <- as.vector (train.test[, 1])
train.test.timestamp <- strptime (train.test.1, "%Y-%m-%d %H:%M:%S")
train.test.year.month.combo <- year.month.combo (train.test.timestamp)

out.of.sample.estimate <- vector (mode="numeric", length=length(train.test.1))

for (i in 1 : length (out.of.sample.estimate)) {
  j <- train.test.year.month.combo[[i]]
  out.of.sample.estimate[[i]] <- exp (combo.count.log.plus1.estimate[[j]]) - 1
}

rmsle (train.test$count, out.of.sample.estimate)

