train.train <- read.table ("train.train.csv", sep=",", row.names=NULL, header=T)
train.test <- read.table ("train.test.csv", sep=",", row.names=NULL, header=T)

# model 6: remove some variables

lm3 <- lm (log.count.plus1 ~ registered + weather
            + workingday + year.angle.sin + year.angle.cos + day.angle.sin + day.angle.cos + year.flag,
            data=train.train.with.new.fields)

summary (lm3)

lm3.prediction <- predict (lm3, train.test.with.new.fields)
lm3.prediction.exp.minus1 <- exp (lm3.prediction) - 1
rmsle (train.test$count, as.vector (lm3.prediction.exp.minus1))

