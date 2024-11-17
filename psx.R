## Pakistan Stock Exchange

library(quantmod)
library(tidyverse)

# Get the data

psx <- read_csv("data/psx.csv")

dim(psx)

glimpse(psx)

## Convert the date column to a date object where month day and year are separated by a hyphen

## psx$date <- as.Date(psx$Date, format = "%m-%d-%Y")

## Why above is incorrect

psx$date <- as.Date(psx$Date, format = "%m/%d/%Y")


glimpse(psx)

## Clean names using janitor

psx <-  janitor::clean_names(psx) |> select(-"date") |>
  rename(date = date_2)

## Create a new column called direction which will be the direction of the stock
## price movement when change_price is greater than 0 the direction is up

psx$direction <- ifelse(psx$change_price > 0, "Up", "Down")

## Convert change_price to a numeric

psx$change_price <- as.numeric(psx$change_price)

psx$change_percent <- as.numeric(gsub("%", "", psx$change_percent))

## Now creat direction column

psx$direction <- ifelse(psx$change_percent > 0, "Up", "Down")

## Arrange the data by date in ascending order

psx <- psx %>% arrange(date)

glimpse(psx)

## Create lag1 to lag5 columns using change_percent

psx <- psx %>% mutate(lag1 = lag(change_percent, 1),
                      lag2 = lag(change_percent, 2),
                      lag3 = lag(change_percent, 3),
                      lag4 = lag(change_percent, 4),
                      lag5 = lag(change_percent, 5))



# Convert `vol` to numeric
psx$vol <- as.numeric(gsub("M", "e6", gsub("K", "e3", psx$vol)))


## Logistic regression

glm.fit <- glm(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + vol,
                data = psx, family = binomial)

psx$direction <- ifelse(psx$direction == "Up", 1, 0)


psx_clean <- psx |> na.omit()

glm_fit <- glm(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + vol,
                data = psx_clean, family = binomial)

summary(glm_fit)


glm.probs=predict(glm_fit,type="response")
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"Up","Down")

table(glm.pred,psx_clean$direction)
mean(glm.pred==psx_clean$direction)

# Make train and test data before 2024 and after 2024

train <- psx_clean$date < as.Date("2024-01-01")




glm_train <- glm(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + vol,
                data = psx_clean[train,], family = binomial)

## Predict on the test data

glm_probs <- predict(glm_train, newdata = psx_clean[!train,], type = "response")

glm_pred <- ifelse(glm_probs > 0.5, "Up", "Down")

table(glm_pred, psx_clean$direction[!train])

mean(glm_pred == psx_clean$direction[!train])

