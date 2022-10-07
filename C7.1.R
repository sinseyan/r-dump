library(tree)
library(ISLR)
library(dplyr)
data <- read.csv("RProjects/Boston.csv")
head(data)
str(data)
glimpse(data)


clean_data <- data %>% 
  mutate(live = ifelse(crim>1,0,1)) %>%
  mutate(live = factor(live, levels = c(0,1), labels = c('No', 'Yes'))) %>% 
  
  na.omit()

glimpse(clean_data)

live.tree <- tree(clean_data$live~.-crim,data = clean_data)
summary(live.tree)

plot(live.tree)
text(live.tree, pretty=0)

set.seed (2)
train = sample(1: nrow( clean_data ), 200)
clean_data.test = clean_data [- train ,]
live.test = clean_data$live [- train ]
tree.clean_data = tree( live~.-crim ,clean_data ,subset = train )
tree.pred= predict (tree.clean_data , clean_data.test , type ="class")
table_mat <- table (tree.pred , live.test )

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

