library(rpart)
library(rpart.plot)
library(dplyr)
path <- 'RProjects/titanic_data.csv'
titanic <-read.csv(path)
head(titanic)

shuffle_index <- sample(1:nrow(titanic))
head(shuffle_index)
ptitanic <- titanic[shuffle_index, ]
head(titanic)


clean_titanic <- titanic %>% 
  select(-c(home.dest, cabin, name, x, ticket)) %>% 
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('Dead', 'Survived')),
         sex = factor(sex),
         embarked = factor(embarked),
         age = as.numeric(sub("?","0",age,fixed = TRUE)),
         fare = as.numeric(sub("?","0",fare,fixed = TRUE))) %>%
  na.omit()

glimpse(clean_titanic)

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

data_train <- create_train_test(clean_titanic, 0.8, train = TRUE)
data_test <- create_train_test(clean_titanic, 0.2, train = FALSE)


fit <- rpart(survived~., data = data_train, method = 'class')
rpart.plot(fit)

predict <-predict(fit, data_test, type = 'class')
actual <- data_test$survived
table_mat <- table(actual,predict)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
