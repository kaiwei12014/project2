train.train <- read.table ("train.train.csv", sep=",", row.names=NULL, header=T)
train.test <- read.table ("train.test.csv", sep=",", row.names=NULL, header=T)

# model 7: only year and day variables

lm4 <- lm (log.count.plus1 ~ + year.angle.sin + year.angle.cos + day.angle.sin + day.angle.cos + year.flag,
            data=train.train.with.new.fields)

summary (lm4)

lm4.prediction <- predict (lm4, train.test.with.new.fields)
lm4.prediction.exp.minus1 <- exp (lm4.prediction) - 1
rmsle (train.test$count, as.vector (lm4.prediction.exp.minus1))

