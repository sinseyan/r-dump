library(tidyverse)
library(ggpubr)

cars <- read.csv('RProjects/mtcars.csv')
head(cars)
glimpse(cars)


glimpse(cars)

model <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, data = cars)
summary(model)
#We can see at the model that the predictors with 50% or higher chance of not being meaningful to the model are:
#cyl, drat, vs, gear, and carb.This assumption is based on the observation of p-values in the model.

model <- lm(mpg ~ disp + hp + wt + qsec + am, data = cars)
summary(model)
#Using this model, the accuracy went up to 84% which proves that the variables:
#disp, hp, wt, qsec, and am
#are meaningful and relevant to the regression

ggplot(cars, aes(x = disp + hp + wt + qsec + am, y = mpg)) +
  geom_point() + 
  stat_smooth(method = lm)