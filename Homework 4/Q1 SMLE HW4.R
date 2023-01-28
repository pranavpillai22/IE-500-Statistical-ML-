#IE 500 SMLE HW 4 Q1 

cases_usa<- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
library(ggplot2)
library(forecast)
library(fpp)

daily_cases <- cases_usa$cases
daily_deaths<- cases_usa$deaths
cases_usa$date <- as.Date(cases_usa$date)
dates_new<- cases_usa[cases_usa$date > "2020-03-01"&
                        cases_usa$date < "2021-11-22",]
dt <- seq(as.Date("2020-03-02"),as.Date("2021-11-21"),by = "days")
ts1 <- zoo(dates_new$cases,dt) 
ts2 <- zoo(dates_new$deaths,dt)
print(ts1)
print(ts2)
ts_total <-merge(ts1,ts2)
plot(ts_total)
plot(diff(ts1))
plot(diff(ts2))
plot(log(ts1))
plot(log(ts2))
plot(diff(log(ts1)))
plot(diff(log(ts2)))
auto.arima(diff(log(ts1)))
auto.arima(diff(log(ts2)))


arima_cases1 <- predict(arima(dates_new$cases, order = c(4,1,2)), n.ahead = 1)
arima_cases1
arima_cases5 <- predict(arima(dates_new$cases, order = c(4,1,2)), n.ahead = 5) 
arima_cases5
arima_cases10 <- predict(arima(dates_new$cases, order = c(4,1,2)), n.ahead = 10) 
arima_cases10

arima_deaths1 <- predict(arima(dates_new$deaths, order = c(4,1,2)), n.ahead = 1)
arima_deaths1
arima_deaths5 <- predict(arima(dates_new$deaths, order = c(4,1,2)), n.ahead = 5) 
arima_deaths5
arima_deaths10 <- predict(arima(dates_new$deaths, order = c(4,1,2)), n.ahead = 10) 
arima_deaths10
library(Metrics)
rmse(dates_new[630,2],arima_cases1$pred)
rmse(dates_new[625:630,2],arima_cases5$pred[1:5])
rmse(dates_new[620:630,2],arima_cases10$pred[1:10])
rmse(dates_new[630,3],arima_deaths1$pred)
rmse(dates_new[625:630,3],arima_deaths5$pred[1:5])
rmse(dates_new[620:630,3],arima_deaths10$pred[1:10])
 