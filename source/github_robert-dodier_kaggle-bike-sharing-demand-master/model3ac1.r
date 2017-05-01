source ("my.functions.r")

train.train <- read.table ("train.train.csv", sep=",", row.names=NULL, header=T)
train.test <- read.table ("train.test.csv", sep=",", row.names=NULL, header=T)

# model 3ac1: estimate = mean of log(target + 1) by hour of day, day type,
# then model residuals of that as residual log(target + 1) ~ linear model fitted to recent residuals
# using lots of variables
# model casual and registered separately

train.train.1 <- as.vector (train.train[, 1])
train.train.timestamp <- strptime (train.train.1, "%Y-%m-%d %H:%M:%S")

combo.n.casual <- array (dim=c(24, 2), data=0)
combo.n.registered <- array (dim=c(24, 2), data=0)
combo.log.plus1.total.casual <- array (dim=c(24, 2), data=0)
combo.log.plus1.total.registered <- array (dim=c(24, 2), data=0)

for (i in 1 : length (train.train.1)) {
  k <- train.train.timestamp$hour[[i]] + 1
  l <- train.train[[i, "workingday"]] + 1
  combo.n.casual[k, l] <- combo.n.casual[k, l] + 1
  combo.n.registered[k, l] <- combo.n.registered[k, l] + 1
  combo.log.plus1.total.casual[k, l] <- combo.log.plus1.total.casual[k, l] + log (train.train$casual[[i]] + 1)
  combo.log.plus1.total.registered[k, l] <- combo.log.plus1.total.registered[k, l] + log (train.train$registered[[i]] + 1)
}

combo.count.log.plus1.estimate.casual <- combo.log.plus1.total.casual / combo.n.casual
combo.count.log.plus1.estimate.registered <- combo.log.plus1.total.registered / combo.n.registered
# if no data for a bin, let estimate = global mean-log
# could assign value in some other way !!
combo.count.log.plus1.estimate.casual [combo.n.casual == 0] <- mean (log (train.train$casual + 1))
combo.count.log.plus1.estimate.registered [combo.n.registered == 0] <- mean (log (train.train$registered + 1))

in.sample.estimate.casual <- vector (mode="numeric", length=length(train.train.1))
in.sample.estimate.registered <- vector (mode="numeric", length=length(train.train.1))

for (i in 1 : length (train.train.1)) {
  k <- train.train.timestamp$hour[[i]] + 1
  l <- train.train[[i, "workingday"]] + 1
  in.sample.estimate.casual[[i]] <- exp (combo.count.log.plus1.estimate.casual[k, l]) - 1
  in.sample.estimate.registered[[i]] <- exp (combo.count.log.plus1.estimate.registered[k, l]) - 1
}
  
in.sample.estimate <- in.sample.estimate.casual + in.sample.estimate.registered
rmsle (train.train$count, in.sample.estimate)

train.test.1 <- as.vector (train.test[, 1])
train.test.timestamp <- strptime (train.test.1, "%Y-%m-%d %H:%M:%S")

out.of.sample.estimate <- vector (mode="numeric", length=length(train.test.1))

for (i in 1 : length (out.of.sample.estimate)) {
  k <- train.test.timestamp$hour[[i]] + 1
  l <- train.test[[i, "workingday"]] + 1
  out.of.sample.estimate[[i]] <- exp (combo.count.log.plus1.estimate[k, l]) - 1
}

rmsle (train.test$count, out.of.sample.estimate)

my.sle <- sle (train.test$count, out.of.sample.estimate)
summary (my.sle)

# model 3a: linear combination of other variables on residuals from preceding model

train.train.with.new.fields <- train.train

combo.count.log.plus1.residuals <- log (train.train$count + 1) - log (in.sample.estimate + 1)

train.train.with.new.fields$model3.residuals <- combo.count.log.plus1.residuals

train.train.with.new.fields$days.elapsed <- vector (mode="numeric", length=length(train.train.timestamp))

lm3ac <- list ()

for (i in 1:24) {
  yyyy0 <- 2010 + (10 + i) %/% 12
  mm0 <- 1 + (10 + i) %% 12
  yyyy1 <- 2010 + (12 + i) %/% 12
  mm1 <- 1 + (12 + i) %% 12
  
  d0 <- sprintf ("%04d-%02d-%02d 00:00:00", yyyy0, mm0, 1)
  d1 <- sprintf ("%04d-%02d-%02d 00:00:00", yyyy1, mm1, 1)
  t0 <- strptime (d0, "%Y-%m-%d %H:%M:%S")
  t1 <- strptime (d1, "%Y-%m-%d %H:%M:%S")

  filter <- train.train.timestamp >= t0 & train.train.timestamp < t1
  train.train.with.new.fields$days.elapsed[filter] <- (as.numeric (train.train.timestamp[filter]) - as.numeric (t0))/24/3600

  print (sprintf ("train %d-th model with %d data from %s to %s", i, sum(filter), d0, d1))
  lm3ac[[i]] <- lm (model3.residuals ~ days.elapsed + atemp + humidity + temp + weather + windspeed,
                    data=train.train.with.new.fields[filter,])
}

# rmsle for combined model in sample
predict.residual.in.sample <- vector (mode="numeric", length=length(train.train.timestamp))

for (i in 1:24) {
  yyyy0 <- 2010 + (10 + i) %/% 12
  mm0 <- 1 + (10 + i) %% 12
  yyyy1 <- 2010 + (12 + i) %/% 12
  mm1 <- 1 + (12 + i) %% 12
  
  d0 <- sprintf ("%04d-%02d-%02d 00:00:00", yyyy0, mm0, 1)
  d1 <- sprintf ("%04d-%02d-%02d 00:00:00", yyyy1, mm1, 1)
  t0 <- strptime (d0, "%Y-%m-%d %H:%M:%S")
  t1 <- strptime (d1, "%Y-%m-%d %H:%M:%S")

  filter <- train.train.timestamp >= t0 & train.train.timestamp < t1
  train.train.with.new.fields$days.elapsed[filter] <- (as.numeric (train.train.timestamp[filter]) - as.numeric (t0))/24/3600

  print (sprintf ("predict %d-th model with %d data from %s to %s", i, sum(filter), d0, d1))
  predict.residual.in.sample[filter] <- predict (lm3ac[[i]], train.train.with.new.fields[filter,])
}

predict.in.sample.combined <- exp (log (in.sample.estimate + 1) + predict.residual.in.sample) - 1

rmsle (train.train$count, predict.in.sample.combined)

# rmsle for combined model out of sample

train.test.with.new.fields <- train.test

train.test.with.new.fields$days.elapsed <- vector (mode="numeric", length=length(train.test.timestamp))

predict.residual.out.of.sample <- vector (mode="numeric", length=length(train.test.timestamp))

for (i in 1:24) {
  yyyy00 <- 2010 + (10 + i) %/% 12
  mm00 <- 1 + (10 + i) %% 12
  yyyy01 <- 2010 + (11 + i) %/% 12
  mm01 <- 1 + (11 + i) %% 12
  yyyy1 <- 2010 + (12 + i) %/% 12
  mm1 <- 1 + (12 + i) %% 12
  
  d00 <- sprintf ("%04d-%02d-%02d 00:00:00", yyyy00, mm00, 1)
  d01 <- sprintf ("%04d-%02d-%02d 00:00:00", yyyy01, mm01, 1)
  d1 <- sprintf ("%04d-%02d-%02d 00:00:00", yyyy1, mm1, 1)
  t00 <- strptime (d00, "%Y-%m-%d %H:%M:%S")
  t01 <- strptime (d01, "%Y-%m-%d %H:%M:%S")
  t1 <- strptime (d1, "%Y-%m-%d %H:%M:%S")

  filter <- train.test.timestamp >= t01 & train.test.timestamp < t1
  train.test.with.new.fields$days.elapsed[filter] <- (as.numeric (train.test.timestamp[filter]) - as.numeric (t00))/24/3600

  print (sprintf ("predict %d-th model with %d data from %s to %s; t00 for days.elapsed=%s", i, sum(filter), d01, d1, d00))
  predict.residual.out.of.sample[filter] <- predict (lm3ac[[i]], train.test.with.new.fields[filter,])
}

predict.out.of.sample.combined <- exp (log (out.of.sample.estimate + 1) + predict.residual.out.of.sample) - 1

rmsle (train.test$count, predict.out.of.sample.combined)

