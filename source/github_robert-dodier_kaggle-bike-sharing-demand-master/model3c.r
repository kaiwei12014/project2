source ("my.functions.r")

train.train <- read.table ("train.train.csv", sep=",", row.names=NULL, header=T)
train.test <- read.table ("train.test.csv", sep=",", row.names=NULL, header=T)

# model 3c: estimate = mean of log(target + 1) by month, hour of day, day type
# combine workingday and holiday to create 3-value day type
# s.t. 1 = holiday, 2 == non-holiday weekend, 3 == working day
# i.e. holiday + 2*workingday
# => 0 = !holiday & !workingday, 1 = holiday & !workingday, 2 = !holiday & workingday, 3 = holiday & workingday
# !holiday & !workingday = weekend; holiday & workingday mutually exclusive
# so: 0 = weekend, 1 = holiday, 2 = workingday
# so: 1 + holiday + 2*workingday => 1 = weekend, 2 = holiday, 3 = workingday

train.train.1 <- as.vector (train.train[, 1])
train.train.timestamp <- strptime (train.train.1, "%Y-%m-%d %H:%M:%S")

train.train.year.month.combo <- year.month.combo (train.train.timestamp)

combo.max <- max (train.train.year.month.combo)
combo.n <- array (dim=c(combo.max, 24, 3), data=0)
combo.log.plus1.total <- array (dim=c(combo.max, 24, 3), data=0)
train.train.1 <- as.vector (train.train[, 1])
train.train.timestamp <- strptime (train.train.1, "%Y-%m-%d %H:%M:%S")

for (i in 1 : length (train.train.1)) {
  j <- train.train.year.month.combo[[i]]
  k <- train.train.timestamp$hour[[i]] + 1
  l <- 1 + train.train[[i, "holiday"]] + 2*train.train[[i, "workingday"]]
  combo.n[j, k, l] <- combo.n[j, k, l] + 1
  combo.log.plus1.total[j, k, l] <- combo.log.plus1.total[j, k, l] + log (train.train$count[[i]] + 1)
}

combo.count.log.plus1.estimate <- combo.log.plus1.total / combo.n
# some holiday bins are empty; assume holidays are same as weekends to fill them
zero.n <- combo.n[,,2] == 0
weekend.estimate <- combo.count.log.plus1.estimate[,,1]
holiday.estimate <- combo.count.log.plus1.estimate[,,2]
holiday.estimate.imputed <- holiday.estimate
holiday.estimate.imputed[zero.n] <- weekend.estimate[zero.n]
combo.count.log.plus1.estimate[,,2] <- holiday.estimate.imputed

# one weekday bin is empty; assume that bin is the same as the one before it
combo.count.log.plus1.estimate[[1, 4, 3]] <- combo.count.log.plus1.estimate[[1, 3, 3]]

in.sample.estimate <- vector (mode="numeric", length=length(train.train.1))

for (i in 1 : length (in.sample.estimate)) {
  j <- train.train.year.month.combo[[i]]
  k <- train.train.timestamp$hour[[i]] + 1
  l <- 1 + train.train[[i, "holiday"]] + 2*train.train[[i, "workingday"]]
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
  l <- 1 + train.test[[i, "holiday"]] + 2*train.test[[i, "workingday"]]
  out.of.sample.estimate[[i]] <- exp (combo.count.log.plus1.estimate[j, k, l]) - 1
}

rmsle (train.test$count, out.of.sample.estimate)
