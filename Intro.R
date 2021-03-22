# Store Item Demand

# Sorry, I'm gonna have so many errors here 

# Libraries
library(forecast)
library(tidyverse)
library(lubridate)

# Read in the data
store.train <- vroom::vroom("~/Downloads/train 2.csv")
store.test <- vroom::vroom("~/Downloads/test (1).csv")
store.test$kind <- "test"
store.train$kind <- "train"
store <- bind_rows(store.test, store.train)

# Number of stores and items
with(store, table(item, store))
# 10 stores, and 50 items

# Sales by month
ggplot(data=store.train %>% filter(item==1),
       mapping=aes(x=month(date) %>% as.factor(), y = sales)) +
  geom_boxplot()

store <- store %>% mutate(month=as.factor(month(date)))

# Month is an important factor at this point

# Sales of the item by stores

ggplot(data=store.train %>% filter(item==17),
       mapping=aes(x=date, y=sales, color=as.factor(store))) +
  geom_line()

# Sales are increasing over year
# There is also a very strong seasonality effect (probably equal to the month effect)

store <- store %>% mutate(year=as.factor(year(date)),
                          time=year(date)+yday(date)/365)

ggplot(data = store %>% filter(item==17, store==7),
       mapping=aes(x=time, y=sales)) +
  geom_line() + geom_smooth(method='lm')

## Linear model with time and month
mt.lm <- lm(sales~month+time, data =(store %>% filter(item==17, store==7)))
fit.vals <- fitted(mt.lm)
plot(x=(store %>% filter(item==17, store==7) %>% pull(time)),
     y=store %>% filter(item==17, store==7) %>% pull(sales), type = 'l')
lines((store %>% filter(item==17, store==7, !is.na(sales)) %>% pull(sales)),
      fit.vals, col = "red", lwd=2)

## Weekend Effect
ggplot(data = store.train %>% filter(item==1),
       mapping=aes(x=wday(date, label=TRUE) %>% as.factor(), y = sales)) +
  geom_boxplot()

## Holiday
# There is a freaking function for that
# Library PROPHET 

ggplot(data = store.train %>% filter(item==17),
       mapping=aes(x=date, y=sales)) +
  geom_line() + facet_wrap(~as.factor(item))

#Weird
ggplot(data = store.train,
       mapping=aes(x=as.factor(item), y=sales)) +
  geom_boxplot() + facet_wrap(~as.factor(store))

ggplot(data = store.train %>% filter(item==17),
       mapping=aes(x=as.factor(item), y=sales)) +
  geom_boxplot()

# SARINA: time series models (yesterday looks like today) YAY FOR TIME SERIES
# (p,d,q,P,D,Q) : lower-case controls day to day (bigger, higher correlation), 
#                 capital letters is seasonal (bigger, higher correlation per season)

#   Arima()
#   Easier: auto.arima()

y <- store.train %>% filter(item==17, store==7) %>%
  pull(sales) %>% ts(data=., start = 1, frequency = 365)
#365 time periods per season?

arima.mod <- auto.arima(y=y, max.p=2, max.q=2, max.d=2)
# the biggest number is 5 (the number of models is searching on)
arima.mod
# It's a forecast

# Predictions

preds <- forecast(arima.mod, 90)

plot(preds)

#predicted <- predict(arima.mod, newdata = (store %>% filter(kind == "test")))


# Do a double for loop (store and item) to do the predictions
summary(predicted)

