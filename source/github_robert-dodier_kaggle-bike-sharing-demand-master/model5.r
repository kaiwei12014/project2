train.train <- read.table ("train.train.csv", sep=",", row.names=NULL, header=T)
train.test <- read.table ("train.test.csv", sep=",", row.names=NULL, header=T)

# model 5: linear combination with interactions

lm2 <- lm (log.count.plus1 ~ atemp + casual + holiday + humidity + registered + season + temp + weather + windspeed
            + workingday + year.angle.sin + year.angle.cos + day.angle.sin + day.angle.cos + year.flag
            + workingday:day.angle.sin + workingday:day.angle.cos,
            data=train.train.with.new.fields)

summary (lm2)
plot (lm2)

lm2.prediction <- predict (lm2, train.test.with.new.fields)
lm2.prediction.exp.minus1 <- exp (lm2.prediction) - 1
rmsle (train.test$count, as.vector (lm2.prediction.exp.minus1))

