library(factoextra)
library(dplyr)

set.seed(123456)
wine <- read.csv("winequality-red.csv")

glimpse(wine)
unique(wine["quality"])

wine <- wine %>%
  mutate(quality = factor(quality, 
                        levels = c(3,4,5,6,7,8), 
                        labels = c('Poor', 'Low' ,'Acceptable', 'Average', 'Good', 'High')))

glimpse(wine)
         
wine_data <- wine[1:11]
glimpse(wine_data)

wine_scale <- scale(wine_data)
wine_data <- dist(wine_scale)
fviz_nbclust(wine_scale, kmeans, method="wss") +
  labs(subtitle="Elbow Method")

unique(wine["quality"])
km.out <- kmeans(wine_scale, centers=6, nstart=100)
print(km.out)

km.clusters <- km.out$cluster
rownames(wine_scale) <- paste(wine$quality, 1:dim(wine)[1], sep = "_")
fviz_cluster(list(data=wine_scale, cluster =km.clusters))
table(km.clusters, wine$quality)

