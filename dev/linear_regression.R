library(tidyverse)
#library(GGally)
housing <- read_csv("housing.csv")

smp_size <- floor(0.75 * nrow(housing))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(housing)), size = smp_size)

train <- housing[train_ind, ]
test <- housing[-train_ind, ]

#ggpairs(housing)

model1 <- lm(median_house_value ~ median_income, data = train)
summary(model1)
model2 <- lm(median_house_value ~ median_income + ocean_proximity, data = train)
summary(model2)
model3 <- lm(median_house_value ~ median_income + ocean_proximity + total_rooms, data = train)
summary(model3)
model4 <- lm(median_house_value ~ median_income + ocean_proximity + total_rooms + housing_median_age, data = train)
summary(model4)
model5 <- lm(median_house_value ~ median_income + ocean_proximity + total_rooms + housing_median_age + longitude * latitude, data = train)
summary(model5)

model6 <- lm(median_house_value ~ median_income + ocean_proximity + total_rooms + housing_median_age + longitude * latitude, data = test)
summary(model6)

