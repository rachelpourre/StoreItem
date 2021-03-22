
# Libraries
library(forecast)
library(tidyverse)
library(lubridate)
library(bestglm)

# Read in the data

store.train <- vroom::vroom("~/Downloads/train 2.csv")
store.test <- vroom::vroom("~/Downloads/test (1).csv")
store.test$kind <- "test"
store.train$kind <- "train"
store.train <- store.train %>% mutate(month=as.factor(month(date)))
store.test <- store.test %>% mutate(month=as.factor(month(date)))
store.train <- store.train %>% mutate(year=(year(date)))
store.test <- store.test %>% mutate(year=(year(date)))

store <- bind_rows(store.test, store.train)

store$item <- as.factor(store$item)
store$store <- as.factor(store$store)

# the independent variable needs to be the last.. And it is
# get rid of date

store <- store[,-c(2)]

# What to do? 
# Go crazy with interactions

store_lm <- lm(sales ~ month + year + item + store + item:store:year, data = store)
summary(store_lm)

# Predict
predicted <- predict(store_lm, newdata = (store %>% filter(kind == "test")))

store_sub <- store %>% filter(kind == "test")

my_sub <- data_frame('id' = store_sub$id, 'sales' = predicted)

#write.table(my_sub, file = "Sub1.csv", sep = ",",row.names = FALSE, col.names=c("id", "sales"))
write_csv(my_sub, 'submission2.csv')

summary(store$sales)







