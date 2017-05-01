train<-read.table("train.csv", row.names=NULL, sep=",", header=T)
summary (train)

plot (train$weather, train$count)
plot (train$temp, train$count)
plot (train$atemp, train$count)
plot (train$humidity, train$count)
plot (train$windspeed, train$count)
plot (train$season, train$count)
plot (train$holiday, train$count)
plot (train$workingday, train$count)

train.1 <- as.vector (train[, 1])
train.timestamp <- strptime (train.1, "%Y-%m-%d %H:%M:%S")
plot (train.timestamp, train$count, type='l')

plot (c(0, 25), c(0, 1000), type='n')
for (i in 1:30) {
  filter <- train.timestamp$year==111 & train.timestamp$mon==6 & train.timestamp$mday==i
  lines (train.timestamp$hour[filter], train$count[filter])
}

plot (c(0, 25), c(0, 1000), type='n')
for (i in 1:30) {
  filter <- train.timestamp$year==111 & train.timestamp$mon==6 & train.timestamp$mday==i
  lines (train.timestamp$hour[filter], train$registered[filter])
}

plot (c(0, 25), c(0, 1000), type='n')
for (i in 1:30) {
  filter <- train.timestamp$year==111 & train.timestamp$mon==6 & train.timestamp$mday==i
  lines (train.timestamp$hour[filter], train$casual[filter])
}

