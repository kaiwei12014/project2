source ("my.functions.r")

train <- read.table ("train.csv", sep=",", row.names=NULL, header=T)

train.1 <- as.vector (train[, 1])
train.timestamp <- strptime (train.1, "%Y-%m-%d %H:%M:%S")
train.year.month.combo <- year.month.combo (train.timestamp)

combo.max <- max (train.year.month.combo)
combo.n <- matrix (nrow=combo.max, ncol=24, data=0)
combo.log.plus1.total <- matrix (nrow=combo.max, ncol=24, data=0)

for (i in 1 : length (train.1)) {
  j <- train.year.month.combo[[i]]
  k <- train.timestamp$hour[[i]] + 1
  combo.n[j, k] <- combo.n[j, k] + 1
  combo.log.plus1.total[j, k] <- combo.log.plus1.total[j, k] + log (train$count[[i]] + 1)
}

combo.count.log.plus1.estimate <- combo.log.plus1.total / combo.n

test <- read.table ("test.csv", sep=",", row.names=NULL, header=T)

test.1 <- as.vector (test[, 1])
test.timestamp <- strptime (test.1, "%Y-%m-%d %H:%M:%S")
test.year.month.combo <- year.month.combo (test.timestamp)

test.estimate <- vector (mode="numeric", length=length(test.1))

for (i in 1 : length (test.estimate)) {
  j <- test.year.month.combo[[i]]
  k <- test.timestamp$hour[[i]] + 1
  test.estimate[[i]] <- exp (combo.count.log.plus1.estimate[j, k]) - 1
}

test.predict <- data.frame (datetime=test$datetime, count=test.estimate)

write.table (test.predict, "test.predict.model2.csv", sep=",", quote=F, row.names=F)
