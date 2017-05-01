#LIBRARY---------------------
require(xgboost)
require(dplyr)
require(data.table)
library(lubridate)
#Pre-Proccessing-------------

setwd("y:/ICT Group/1.Documentation/Network & Security/Moa/ForestCoverType/Bike sharing/")


train=read.csv("train.csv")
test=read.csv("test.csv")
str(train)


test$registered=0
test$casual=0
test$count=0
data=rbind(train,test)

str(data)

summary(data)

data$season=as.factor(data$season)
data$weather=as.factor(data$weather)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)

data$hour=substr(data$datetime,12,13)
data$hour=as.factor(data$hour)

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

boxplot(train$count~train$hour,xlab="hour", ylab="count of users")
boxplot(train$casual~train$hour,xlab="hour", ylab="casual users")
boxplot(train$registered~train$hour,xlab="hour", ylab="registered users")

date=substr(data$datetime,1,10)
days<-weekdays(as.Date(date))
data$day=days

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

boxplot(train$registered~train$day,xlab="day", ylab="registered users")
boxplot(train$casual~train$day,xlab="day", ylab="casual users")

boxplot(train$registered~train$weather,xlab="weather", ylab="registered users")
boxplot(train$casual~train$weather,xlab="weather", ylab="casual users")

boxplot(train$registered~train$temp,xlab="temp", ylab="registered users")
boxplot(train$casual~train$temp,xlab="temp", ylab="casual users")


data$year=substr(data$datetime,1,4)
data$year=as.factor(data$year)

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

boxplot(train$registered~train$year,xlab="year", ylab="registered users")
boxplot(train$casual~train$year,xlab="year", ylab="casual users")


boxplot(train$registered~train$windspeed,xlab="year", ylab="registered users")
boxplot(train$casual~train$windspeed,xlab="year", ylab="casual users")

boxplot(train$registered~train$humidity,xlab="humidity", ylab="registered users")
boxplot(train$casual~train$humidity,xlab="humidity", ylab="casual users")

data$hour=as.integer(data$hour)

data$day_part=0



train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]



data=rbind(train,test)
library(rpart)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

d=rpart(registered~hour,data=train)
fancyRpartPlot(d)

d=rpart(casual~hour,data=train)
fancyRpartPlot(d)

data=rbind(train,test)
data$dp_reg=0
data$dp_reg[data$hour<6]=1
data$dp_reg[data$hour>=22 | data$hour==6]=2
data$dp_reg[data$hour>8 & data$hour<17|data$hour>19 & data$hour<=21]=3
data$dp_reg[data$hour==8| data$hour==17| data$hour==18]=4
data$dp_reg[data$hour==19 | data$hour==7]=5

data$dp_cas=0
data$dp_cas[data$hour<=8 | data$hour>21]=1
data$dp_cas[data$hour==9 & data$hour==20 & data$hour==21 ]=2
data$dp_cas[data$hour>=10 & data$hour<=19]=3

f=rpart(registered~temp,data=train)
fancyRpartPlot(f)


f=rpart(casual~temp,data=train)
fancyRpartPlot(f)

data$temp_reg=0
data$temp_reg[data$temp<13]=1
data$temp_reg[data$temp>=13 & data$temp<23]=2
data$temp_reg[data$temp>=23 & data$temp<30]=3
data$temp_reg[data$temp>=30]=4



data$temp_cas=0
data$temp_cas[data$temp<15]=1
data$temp_cas[data$temp>=15 & data$temp<23]=2
data$temp_cas[data$temp>=23 & data$temp<30]=3
data$temp_cas[data$temp>=30]=4



data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$month>3]=2
data$year_part[data$year=='2011' & data$month>6]=3
data$year_part[data$year=='2011' & data$month>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$month>3]=6
data$year_part[data$year=='2012' & data$month>6]=7
data$year_part[data$year=='2012' & data$month>9]=8
table(data$year_part)



data$day_type=0

data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"


train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

plot(train$temp,train$count)

data=rbind(train,test)
data$month=substr(data$datetime,6,7)

data$month=as.integer(data$month)

table(data$windspeed==0)
k=data$windspeed==0
wind_0=subset(data,k)
wind_1=subset(data,!k)

library(randomForest)

set.seed(415)
fit <- randomForest(windspeed ~ season+weather +humidity +month+temp+ year+atemp, data=wind_1,importance=TRUE, ntree=250)
pred=predict(fit,wind_0)
wind_0$windspeed=pred

data=rbind(wind_0,wind_1)

data$weekend=0
data$weekend[data$day=="Sunday" | data$day=="Saturday"]=1


str(data)

data$season=as.factor(data$season)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)
data$weather=as.factor(data$weather)
data$hour=as.factor(data$hour)
data$month=as.factor(data$month)
data$day_part=as.factor(data$dp_cas)
data$day_type=as.factor(data$dp_reg)
data$day=as.factor(data$day)
data$temp_cas=as.factor(data$temp_cas)
data$temp_reg=as.factor(data$temp_reg)

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]


train$reg1=train$registered+1
train$cas1=train$casual+1
train$logcas=log(train$cas1)
train$logreg=log(train$reg1)
test$logreg=0
test$logcas=0
train$dday = NULL
test$dday = NULL

#CASUAL-------------------------------------------------------------------------------

labelcasual = train[,'logcas']

features = c("season","holiday","workingday","weather","atemp","humidity","windspeed",             
             "day","year","hour","year_part","weekend","dp_cas","day_type","temp_cas")


#------------------------------------------------------------------------------------------
dtrain = train[,features]

dtrain.casual = xgb.DMatrix(data.matrix(dtrain),label=labelcasual)
dtest = data.matrix(test[,features])
X_train = as.matrix(train[,features])
#------------------------------------------------------------------------------------------

model = xgb.train(objective = "reg:linear",data = dtrain.casual, nround = 550,
                  max_depth = 7, eta = 0.01, subsample = 0.8)
#------------------------------------------------------------------------------------------

preds = predict(model, dtest)
preds = expm1(preds)
test$logcas = preds
#REGISTERED-------------------------------------------------------------------------------

labelreg = train[,'logreg']

features = c("season","holiday","workingday","weather","atemp","humidity","windspeed",             
             "day","year","hour","year_part","weekend","dp_reg","day_type","temp_reg")

#------------------------------------------------------------------------------------------
dtrain = train[,features]

dtrain.reg = xgb.DMatrix(data.matrix(dtrain),label=labelreg)
dtest = data.matrix(test[,features])
X_train = as.matrix(train[,features])
#------------------------------------------------------------------------------------------

model = xgb.train(objective = "reg:linear",data = dtrain.reg, nround = 600,
                  max_depth = 11, eta = 0.01, subsample = 0.8)
#------------------------------------------------------------------------------------------

preds = predict(model, dtest)
preds = expm1(preds)
test$logreg = preds
test$count = test$logcas+test$logreg
#WRITE----------------------------------------------------------------------------------

solution = data.frame(datetime = test$datetime, count = round(test$count))
write.csv(solution, "d:/solution.csv", row.names = FALSE)