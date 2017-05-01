source ("my.functions.r")

train <- read.table ("train.csv", sep=",", row.names=NULL, header=T)
test <- read.table ("test.csv", sep=",", row.names=NULL, header=T)

# model 3c: estimate = mean of log(target + 1) by month, hour of day, day type
# combine workingday and holiday to create 3-value day type
# s.t. 1 = holiday, 2 == non-holiday weekend, 3 == working day
# i.e. holiday + 2*workingday
# => 0 = !holiday & !workingday, 1 = holiday & !workingday, 2 = !holiday & workingday, 3 = holiday & workingday
# !holiday & !workingday = weekend; holiday & workingday mutually exclusive
# so: 0 = weekend, 1 = holiday, 2 = workingday
# so: 1 + holiday + 2*workingday => 1 = weekend, 2 = holiday, 3 = workingday

train.1 <- as.vector (train[, 1])
train.timestamp <- strptime (train.1, "%Y-%m-%d %H:%M:%S")

train.year.month.combo <- year.month.combo (train.timestamp)

combo.max <- max (train.year.month.combo)
combo.n <- array (dim=c(combo.max, 24, 3), data=0)
combo.log.plus1.total <- array (dim=c(combo.max, 24, 3), data=0)
train.1 <- as.vector (train[, 1])
train.timestamp <- strptime (train.1, "%Y-%m-%d %H:%M:%S")

for (i in 1 : length (train.1)) {
  j <- train.year.month.combo[[i]]
  k <- train.timestamp$hour[[i]] + 1
  l <- 1 + train[[i, "holiday"]] + 2*train[[i, "workingday"]]
  combo.n[j, k, l] <- combo.n[j, k, l] + 1
  combo.log.plus1.total[j, k, l] <- combo.log.plus1.total[j, k, l] + log (train$count[[i]] + 1)
}

combo.count.log.plus1.estimate <- combo.log.plus1.total / combo.n
# some holiday bins are empty; assume holidays are same as weekends to fill them
zero.n <- combo.n[,,2] == 0
weekend.estimate <- combo.count.log.plus1.estimate[,,1]
holiday.estimate <- combo.count.log.plus1.estimate[,,2]
holiday.estimate.imputed <- holiday.estimate
holiday.estimate.imputed[zero.n] <- weekend.estimate[zero.n]
combo.count.log.plus1.estimate[,,2] <- holiday.estimate.imputed

in.sample.estimate <- vector (mode="numeric", length=length(train.1))

for (i in 1 : length (in.sample.estimate)) {
  j <- train.year.month.combo[[i]]
  k <- train.timestamp$hour[[i]] + 1
  l <- 1 + train[[i, "holiday"]] + 2*train[[i, "workingday"]]
  in.sample.estimate[[i]] <- exp (combo.count.log.plus1.estimate[j, k, l]) - 1
}
  
rmsle (train$count, in.sample.estimate)

test.1 <- as.vector (test[, 1])
test.timestamp <- strptime (test.1, "%Y-%m-%d %H:%M:%S")
test.year.month.combo <- year.month.combo (test.timestamp)

out.of.sample.estimate <- vector (mode="numeric", length=length(test.1))

for (i in 1 : length (out.of.sample.estimate)) {
  j <- test.year.month.combo[[i]]
  k <- test.timestamp$hour[[i]] + 1
  l <- 1 + test[[i, "holiday"]] + 2*test[[i, "workingday"]]
  out.of.sample.estimate[[i]] <- exp (combo.count.log.plus1.estimate[j, k, l]) - 1
}

test.predict <- data.frame (datetime=test$datetime, count=out.of.sample.estimate)

write.table (test.predict, "test.predict.model3c.csv", sep=",", quote=F, row.names=F)
