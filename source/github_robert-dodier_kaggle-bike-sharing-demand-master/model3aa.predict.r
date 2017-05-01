source ("my.functions.r")

# construct base model

train <- read.table ("train.csv", sep=",", row.names=NULL, header=T)

train.1 <- as.vector (train[, 1])
train.timestamp <- strptime (train.1, "%Y-%m-%d %H:%M:%S")

combo.n <- array (dim=c(24, 2), data=0)
combo.log.plus1.total <- array (dim=c(24, 2), data=0)

for (i in 1 : length (train.1)) {
  k <- train.timestamp$hour[[i]] + 1
  l <- train[[i, "workingday"]] + 1
  combo.n[k, l] <- combo.n[k, l] + 1
  combo.log.plus1.total[k, l] <- combo.log.plus1.total[k, l] + log (train$count[[i]] + 1)
}

combo.count.log.plus1.estimate <- combo.log.plus1.total / combo.n
# if no data for a bin, let estimate = global mean-log
# could assign value in some other way !!
combo.count.log.plus1.estimate [combo.n == 0] <- mean (log (train$count + 1))

# in sample predictions for base model

in.sample.estimate <- vector (mode="numeric", length=length(train.1))

for (i in 1 : length (in.sample.estimate)) {
  k <- train.timestamp$hour[[i]] + 1
  l <- train[[i, "workingday"]] + 1
  in.sample.estimate[[i]] <- exp (combo.count.log.plus1.estimate[k, l]) - 1
}
  
print (sprintf ("rmsle for base model in sample = %f", rmsle (train$count, in.sample.estimate)))

# construct combined model

train.with.new.fields <- train

combo.count.log.plus1.residuals <- log (train$count + 1) - log (in.sample.estimate + 1)

train.with.new.fields$model3.residuals <- combo.count.log.plus1.residuals

train.with.new.fields$days.elapsed <- vector (mode="numeric", length=length(train.timestamp))

lm3aa <- list ()

for (i in 1:24) {
  yyyy0 <- 2010 + (10 + i) %/% 12
  mm0 <- 1 + (10 + i) %% 12
  yyyy1 <- 2010 + (12 + i) %/% 12
  mm1 <- 1 + (12 + i) %% 12
  
  d0 <- sprintf ("%04d-%02d-%02d 00:00:00", yyyy0, mm0, 1)
  d1 <- sprintf ("%04d-%02d-%02d 00:00:00", yyyy1, mm1, 1)
  t0 <- strptime (d0, "%Y-%m-%d %H:%M:%S")
  t1 <- strptime (d1, "%Y-%m-%d %H:%M:%S")

  filter <- train.timestamp >= t0 & train.timestamp < t1
  train.with.new.fields$days.elapsed[filter] <- (as.numeric (train.timestamp[filter]) - as.numeric (t0))/24/3600

  print (sprintf ("train %d-th model with %d data from %s to %s", i, sum(filter), d0, d1))
  lm3aa[[i]] <- lm (model3.residuals ~ days.elapsed, data=train.with.new.fields[filter,])
}

# predictions for combined model in sample

predict.residual.in.sample <- vector (mode="numeric", length=length(train.timestamp))

for (i in 1:24) {
  yyyy0 <- 2010 + (10 + i) %/% 12
  mm0 <- 1 + (10 + i) %% 12
  yyyy1 <- 2010 + (12 + i) %/% 12
  mm1 <- 1 + (12 + i) %% 12
  
  d0 <- sprintf ("%04d-%02d-%02d 00:00:00", yyyy0, mm0, 1)
  d1 <- sprintf ("%04d-%02d-%02d 00:00:00", yyyy1, mm1, 1)
  t0 <- strptime (d0, "%Y-%m-%d %H:%M:%S")
  t1 <- strptime (d1, "%Y-%m-%d %H:%M:%S")

  filter <- train.timestamp >= t0 & train.timestamp < t1
  train.with.new.fields$days.elapsed[filter] <- (as.numeric (train.timestamp[filter]) - as.numeric (t0))/24/3600

  print (sprintf ("predict %d-th model with %d data from %s to %s", i, sum(filter), d0, d1))
  predict.residual.in.sample[filter] <- predict (lm3aa[[i]], train.with.new.fields[filter,])
}

predict.in.sample.combined <- exp (log (in.sample.estimate + 1) + predict.residual.in.sample) - 1

print (sprintf ("rmsle for combined model in sample = %f", rmsle (train$count, predict.in.sample.combined)))

# predictions for base model on test data

test <- read.table ("test.csv", sep=",", row.names=NULL, header=T)

test.1 <- as.vector (test[, 1])
test.timestamp <- strptime (test.1, "%Y-%m-%d %H:%M:%S")

out.of.sample.estimate <- vector (mode="numeric", length=length(test.1))

for (i in 1 : length (out.of.sample.estimate)) {
  k <- test.timestamp$hour[[i]] + 1
  l <- test[[i, "workingday"]] + 1
  out.of.sample.estimate[[i]] <- exp (combo.count.log.plus1.estimate[k, l]) - 1
}

# predictions for combined model on test data

test.with.new.fields <- test

test.with.new.fields$days.elapsed <- vector (mode="numeric", length=length(test.timestamp))

predict.residual.out.of.sample <- vector (mode="numeric", length=length(test.timestamp))

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

  filter <- test.timestamp >= t01 & test.timestamp < t1
  test.with.new.fields$days.elapsed[filter] <- (as.numeric (test.timestamp[filter]) - as.numeric (t00))/24/3600

  print (sprintf ("predict %d-th model with %d data from %s to %s; t00 for days.elapsed=%s", i, sum(filter), d01, d1, d00))
  predict.residual.out.of.sample[filter] <- predict (lm3aa[[i]], test.with.new.fields[filter,])
}

predict.out.of.sample.combined <- exp (log (out.of.sample.estimate + 1) + predict.residual.out.of.sample) - 1

test.predict <- data.frame (datetime=test$datetime, count=predict.out.of.sample.combined)

write.table (test.predict, "test.predict.model3aa.csv", sep=",", quote=F, row.names=F)

