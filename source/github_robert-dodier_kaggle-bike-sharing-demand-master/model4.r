train.train <- read.table ("train.train.csv", sep=",", row.names=NULL, header=T)
train.test <- read.table ("train.test.csv", sep=",", row.names=NULL, header=T)

# model 4: linear combination of all variables
# replace timestamp by sin, cos of year angle and 0/1 for year (2011/2012)

train.train.1 <- as.vector (train.train[, 1])
train.train.timestamp <- strptime (train.train.1, "%Y-%m-%d %H:%M:%S")
t <- as.numeric (train.train.timestamp)
year.angle <- (t - t[[1]]) / (365 * 86400) * (2 * pi)

day.angle <- (train.train.timestamp$hour + train.train.timestamp$min/60 + train.train.timestamp$sec/3600) / 24 * (2 * pi)

train.train.with.new.fields$year.angle.sin <- sin (year.angle)
train.train.with.new.fields$year.angle.cos <- cos (year.angle)
train.train.with.new.fields$day.angle.sin <- sin (day.angle)
train.train.with.new.fields$day.angle.cos <- cos (day.angle)
train.train.with.new.fields$year.flag <- train.train.timestamp$year - 111

lm1 <- lm (log.count.plus1 ~ atemp + casual + holiday + humidity + registered + season + temp + weather + windspeed
            + workingday + year.angle.sin + year.angle.cos + day.angle.sin + day.angle.cos + year.flag,
            data=train.train.with.new.fields)

summary (lm1)
plot (lm1)

train.test.1 <- as.vector (train.test[, 1])
train.test.timestamp <- strptime (train.test.1, "%Y-%m-%d %H:%M:%S")
train.test.year.month.combo <- year.month.combo (train.test.timestamp)

t <- as.numeric (train.test.timestamp)
year.angle <- (t - t[[1]]) / (365 * 86400) * (2 * pi)

day.angle <- (train.test.timestamp$hour + train.test.timestamp$min/60 + train.test.timestamp$sec/3600) / 24 * (2 * pi)

train.test.with.new.fields <- train.test

train.test.with.new.fields$year.angle.sin <- sin (year.angle)
train.test.with.new.fields$year.angle.cos <- cos (year.angle)
train.test.with.new.fields$day.angle.sin <- sin (day.angle)
train.test.with.new.fields$day.angle.cos <- cos (day.angle)
train.test.with.new.fields$year.flag <- train.test.timestamp$year - 111

lm1.prediction <- predict (lm1, train.test.with.new.fields)
lm1.prediction.exp.minus1 <- exp (lm1.prediction) - 1
rmsle (train.test$count, as.vector (lm1.prediction.exp.minus1))

plot.new ()
par (fg='red')
plot (as.vector (lm1.prediction.exp.minus1))
lines (as.vector (lm1.prediction.exp.minus1))
par (fg='green')
lines (train.test$count)
zm ()

