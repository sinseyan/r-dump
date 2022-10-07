library(randomForest)
library(caTools)
library(dplyr)
library(ggplot2)



data <- read.csv("car_evaluation.csv")

glimpse(data)
summary(data)
head(data)

renamed_data <- data %>%
  rename(price = vhigh) %>% 
  rename(maintenance = vhigh.1) %>% 
  rename(door = X2) %>% 
  rename(person = X2.1) %>%
  rename(trunk = small) %>% 
  rename(safety = low) %>% 
  rename(decision = unacc)

set.seed(123456)
shuffle_index <- sample(1:nrow(renamed_data))
head(shuffle_index)
renamed_data <- renamed_data[shuffle_index, ]
head(renamed_data)


cleaned_data <- renamed_data %>%
  mutate(price = factor(price, 
                        levels = c('low','med','high','vhigh'), 
                        labels = c('Low', 'Medium' ,'High', 'Very High')),
         maintenance = factor(maintenance, 
                              levels = c('low','med','high','vhigh'), 
                              labels = c('Low', 'Medium' ,'High', 'Very High')),
         door = factor(door, 
                       levels = c('2','3','4','5more'), 
                       labels = c('Two', 'Three' ,'Four', '5 or more')),
         person = factor(person,
                         levels = c('2','4','more'),
                         labels = c('Two', 'Four', '5 or more')),
         trunk = factor(trunk),
         safety = factor(safety),
         decision = factor(ifelse(decision=='unacc',"No",
                                  ifelse(decision!='acc','Yes',
                                         ifelse(price=='vhigh'|price=='high'|maintenance=='vhigh'|maintenance=='high','No','Yes'))))) %>% 
  na.omit()

glimpse(cleaned_data)


summary(cleaned_data)

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

glimpse(cleaned_data)

data_train <- create_train_test(cleaned_data, 0.8, train = TRUE)
data_test <- create_train_test(cleaned_data, 0.8, train = FALSE)

rf <- randomForest(decision~.,data=data_train)
rf

predict <-predict(rf, data_test, type = 'class')
actual <- data_test$decision
table_mat <- table(actual,predict)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


