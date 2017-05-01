source ("my.functions.r")

train <- read.table ("train.csv", sep=",", row.names=NULL, header=T)

train.1 <- as.vector (train[, 1])
train.timestamp <- strptime (train.1, "%Y-%m-%d %H:%M:%S")

train.year.month.combo <- year.month.combo (train.timestamp)
combo.max <- max (train.year.month.combo)
combo.n <- vector (mode="numeric", length=combo.max)
combo.log.plus1.total <- vector (mode="numeric", length=combo.max)

for (i in 1 : length (train.1)) {
  j <- train.year.month.combo[[i]]
  combo.n[[j]] <- combo.n[[j]] + 1
  combo.log.plus1.total[[j]] <- combo.log.plus1.total[[j]] + log (train$count[[i]] + 1)
}

combo.count.log.plus1.estimate <- combo.log.plus1.total / combo.n

test <- read.table ("test.csv", sep=",", row.names=NULL, header=T)

test.1 <- as.vector (test[, 1])
test.timestamp <- strptime (test.1, "%Y-%m-%d %H:%M:%S")
test.year.month.combo <- year.month.combo (test.timestamp)

test.estimate <- vector (mode="numeric", length=length(test.1))

for (i in 1 : length (test.estimate)) {
  j <- test.year.month.combo[[i]]
  test.estimate[[i]] <- exp (combo.count.log.plus1.estimate[[j]]) - 1
}

test.predict <- data.frame (datetime=test$datetime, count=test.estimate)

write.table (test.predict, "test.predict.model1.csv", sep=",", quote=F, row.names=F)
