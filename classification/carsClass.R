library(tree)
library(rpart)
library(rpart.plot)
library(dplyr)


set.seed(123456)
path <- 'car_evaluation.csv'
car <-read.csv(path)

shuffle_index <- sample(1:nrow(car))
head(shuffle_index)
car <- car[shuffle_index, ]
head(car)

head(car)
glimpse(car)

renamed_car <- car %>%
  rename(price = vhigh) %>% 
  rename(maintenance = vhigh.1) %>% 
  rename(door = X2) %>% 
  rename(person = X2.1) %>%
  rename(trunk = small) %>% 
  rename(safety = low) %>% 
  rename(class = unacc)



glimpse(renamed_car)

unique(renamed_car["price"])
unique(renamed_car["maintenance"])
unique(renamed_car["door"])
unique(renamed_car["person"])
unique(renamed_car["trunk"])
unique(renamed_car["safety"])
unique(renamed_car["class"])


cleaned_data <- renamed_car %>%
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
         class = factor(class),
         decision = factor(ifelse(class=='unacc',"No",
                                  ifelse(class!='acc','Yes',
                                         ifelse(price=='vhigh'|price=='high'|maintenance=='vhigh'|maintenance=='high','No','Yes'))))) %>% 
  na.omit()

glimpse(cleaned_data)


car.tree <- tree(cleaned_data$decision~.-class,data = cleaned_data)
summary(car.tree)

plot(car.tree)
text(car.tree, pretty=0)

cleaned_data <- cleaned_data %>% 
  select(-c(class)) 


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

fit <- rpart(decision~., data = data_train, method = 'class')
rpart.plot(fit)

predict <-predict(fit, data_test, type = 'class')
actual <- data_test$decision
table_mat <- table(actual,predict)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

