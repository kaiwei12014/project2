library(xts)
library(gbm)

# load train csv
# train <- read.csv("train.csv", stringsAsFactors=FALSE)
# head(train)
# 
# train$season <- factor(train$season, c(1,2,3,4), ordered=FALSE)
# train$holiday <- factor(train$holiday, c(0,1), ordered=FALSE)
# train$workingday <- factor(train$workingday, c(0,1), ordered=FALSE)
# train$weather <- factor(train$weather, c(4,3,2,1), ordered=TRUE)
# # set datetime ####
# train$datetime <- as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M:%S")
# str(train)

train.lm <- lm(count ~ ., data=train)
summary(train.lm)

#test <- read.csv("test.csv")

genmod<-gbm(count~.
            ,data=train[,-1] ## registered,casual,count columns
            ,var.monotone=NULL # which vars go up or down with target
            ,distribution="gaussian"
            ,n.trees=1200
            ,shrinkage=0.05
            ,interaction.depth=3
            ,bag.fraction = 0.5
            ,train.fraction = 1
            ,n.minobsinnode = 10
            ,cv.folds = 10
            ,keep.data=TRUE
            ,verbose=TRUE)

best.iter <- gbm.perf(genmod,method="cv") ##the best iteration number
print(pretty.gbm.tree(genmod, best.iter))
summary(genmod, n.trees=best.iter)

pred.test <- predict(genmod, test[,-c(1,9)], best.iter, type="response")
summary(pred.test)
pred.test[pred.test<0] <- 0