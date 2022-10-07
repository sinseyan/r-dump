library(tidyverse)
library(ggpubr)
theme_set(theme_pubr())
data("marketing", package = "datarium") 
head(marketing, 4)


ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point()


cor(marketing$sales, marketing$youtube)


model <- lm(sales ~ youtube, data = marketing)
model

ggplot(marketing, aes(youtube, sales)) +
  geom_point() +
  stat_smooth(method = lm)


summary(model)

confint(model)

sigma(model)*100/mean(marketing$sales)
