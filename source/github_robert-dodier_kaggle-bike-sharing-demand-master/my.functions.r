rmsle <- function (y, y.hat) { sqrt(sum((log(y.hat + 1) - log(y + 1))**2)/length(y)) }

rmse <- function (y, y.hat) { sqrt(sum((y.hat - y)**2)/length(y)) }

year.month.combo <- function (d) { 12*(d$year - 111) + (d$mon + 1) }

sle <- function (y, y.hat) { (log(y.hat + 1) - log(y + 1))**2 }

