print("Running EDA.")

summary(train)
#no missing values

#by categorical variable
layout(matrix(c(1,2,3,4),2,2,byrow=FALSE))
#boxplot(train$count, main="Total Hourly Ridership")
boxplot(train$count ~ train$weather, main="Ridership vs. Weather", ylab="Ridership", xlab="Weather")
boxplot(train$count ~ train$season, main="Ridership vs. Season", ylab="Ridership", xlab="Season")
boxplot(train$count ~ train$holiday, main="Ridership vs. Holiday", ylab="Ridership", xlab="Holiday")
boxplot(train$count ~ train$workingday, main="Ridership vs. Workingday", ylab="Ridership", xlab="Workingday")

# by numeric var
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
plot(train$temp, train$count, main="Ridership vs. Temperature", ylab="Ridership", xlab="Temperature")
plot(train$atemp, train$count, main="Ridership vs. Apparent Temperature", ylab="Ridership", xlab="Apparent Temperature")
plot(train$windspeed, train$count, main="Ridership vs. Windspeed", ylab="Ridership", xlab="Windspeed")
plot(train$humidity, train$count, main="Ridership vs. Humidity", ylab="Ridership", xlab="Humidity")

bartlett.test(count ~ season, data=train)
oneway.test(count ~ season, data=train) #keep Welch's correction

bartlett.test(count ~ holiday, data=train)
oneway.test(count ~ holiday, data=train) #keep Welch's correction

bartlett.test(count ~ workingday, data=train)
oneway.test(count ~ workingday, data=train) #keep Welch's correction

#the one observation for weather==4 throws this off; ignore it here
bartlett.test(train$count ~ weather, data=train)
oneway.test(count ~ weather, data=train) #keep Welch's correction

#season and weather have statistically different means; holiday and workingday don't

summary(lm(count ~ temp, data=train))
summary(lm(count ~ atemp, data=train))
summary(lm(count ~ humidity, data=train))
summary(lm(count ~ windspeed, data=train))


#temp and feelsliketemp will be highly correlated
cor(train$temp, train$atemp)
#cor(train[,6:9])

par(mfrow=c(1,1))
plot(count ~ datetime, data=train, xlab="Year", ylab="Total Ridership", main="Ridership vs. Year")

cor(cbind(casual, registered, count=train$count)) #casual and registered aren't super correlated

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

#pairs(~count+season+holiday+workingday+weather+temp+humidity+windspeed,
#      data=train, 
#      main="Bike Sharing",
#      upper.panel=panel.smooth, 
#      lower.panel=panel.cor)

#temperature varies by season of course (humidity does a bit)
#humidity varies with weather of course
#no huge relations between temp, windspeed, and humidity

par(mfrow=c(1,3))
hist(train$count, xlab="Ridership", main="Histogram of Ridership")
hist(train$count^(1/2), xlab="Transform of Ridership", main="Histogram of Ridership Transform")
hist(log(train$count+1), xlab="Transform of Ridership", main="Histogram of Ridership Transform")

par(mfrow=c(1,1))
plot(count ~ datetime, data=train, type="l", lwd=0.7, main="Count of Bike Rentals")
#likely a seaonsal component but there is definitely an upward trend; need some sort of time series analysis

#Note the gaps in between the months- they are the days after the
#19th of each month. They are the entries we aim to predict with
#our model from the first 19 days.

percentage = registered/train$count*100
plot(percentage ~ train$datetime, main="Percentage of Registered Users", xlab="Year", ylab="Percentage")
#bit more registered users across time

summary(percentage)

par(mfrow=c(1,2))
p = seq(1,100, by=1)
a = rep(40,100)
rmsle = ((log(p+1)-log(a+1))^2)^0.5
rmse = ((p-a)^2)^0.5
plot(p,rmsle, type="l", main="Basic RMSLE Pattern", xlab="Predicted", ylab="RMSLE")
plot(p, rmse, type="l", main="Basic RMSE Pattern", xlab="Predicted", ylab="RMSE")
