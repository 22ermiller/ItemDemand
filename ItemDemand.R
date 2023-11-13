library(tidyverse)
library(patchwork)
library(timetk)
library(vroom)

## Read in Data
test <- vroom('test.csv')
train <- vroom('train.csv')

# Create data sets filtered to a random store and item

store1_item3 <- train %>%
  filter(store == 1 & item == 3)

store5_item7 <- train %>%
  filter(store == 5 & item == 7)

store6_item17 <- train %>%
  filter(store == 6 & item == 17)

store9_item22 <- train %>%
  filter(store == 9 & item == 22)

# Autocorrelation Function Plots

p1 <- store1_item3 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 2*365) +
  ggtitle("Store 1 Item 3")

p2 <- store5_item7 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 2*365) +
  ggtitle("Store 5 Item 7")

p3 <- store6_item17 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 2*365) +
  ggtitle("Store 6 Item 17")


p4 <- store9_item22 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 2*365) +
  ggtitle("Store 9 Item 22")

(p1 + p2) / (p3 + p4)
