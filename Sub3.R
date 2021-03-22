
# Libraries
library(forecast)
library(tidyverse)
library(lubridate)
library(bestglm)

# Read in the data

store.train <- vroom::vroom("~/Downloads/train 2.csv")
store.test <- vroom::vroom("~/Downloads/test (1).csv")
store.test$kind <- "1"
store.train$kind <- "0"
store.train <- store.train %>% mutate(month=(month(date)))
store.test <- store.test %>% mutate(month=(month(date)))
store.train <- store.train %>% mutate(year=(year(date)))
store.test <- store.test %>% mutate(year=(year(date)))

store <- bind_rows(store.test, store.train)

# the independent variable needs to be the last.. And it is
# get rid of date

store <- store[,-c(2)]

# What to do? 

# for (y in 2017) {
#   for (i in 1:10) {
#     for (j in 1:50) {
#         store$magic[store$store == i & store$item == j & store$year == y & !is.na(store$sales)] <- 
#           median(store$sales[store$store == i & store$item == j & store$year == y & !is.na(store$sales)]) 
#     }
#   }
# }
# Silly error


#store$magic <- as.factor(store$magic)

#store_sub <- store %>% filter(kind == "train")

#store_lm <- lm(sales ~ month + item + store + item:store + item:month, data = store)
# This could be the third submission (and honestly kinda fun), but let's try somthing different
#summary(store_lm)

store$item <- as.factor(store$item)
store$store <- as.factor(store$store)
store$month <- as.factor(store$month)
# store$year <- as.factor(store$year)

for (s in 1:10) {
  for (i in 1:50) {
    #modeldata <- store[(store$item = i) & (store$store = s)]
    model_ <- lm(sales ~ month + year, data = store[(store$item = i) & (store$store = s)])
                   #(store %>% filter((store$item == i) & (store$store == s) & (store$kind == "train"))))
                    #store[(store$item == i) & (store$store == s) & (store$kind == "train")])
    #store$sales[store$kind == "test"] <- predict(model_, newdata = (store %>% filter(kind == "test")))
    preds <- predict(model_, newdata = store %>% filter(kind == 1))
    store$sales[(store$item = i) & (store$store = s) & (store$kind == 1)] <- preds
  }
}


# Predict
#predicted <- predict(store_lm, newdata = (store %>% filter(kind == "test")))

store_sub <- store %>% filter(kind == 1)

my_sub <- data_frame('id' = store_sub$id, 'sales' = store_sub$sales)

write_csv(my_sub, 'submission.csv')

