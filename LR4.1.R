library(tidyverse)
library(ggpubr)
library(dplyr)

states <- readRDS('RProjects/states.rds')
summary(states)
glimpse(states)

ggplot(states, aes(x = metro, y = energy)) +
  geom_point() + 
  stat_smooth(method = lm)

model <- lm(energy ~ metro + pop + area + density + waste + miles + toxic + green + house, data = states)
summary(model)
#We can see at the model that the two (2) lowest p-values are toxic and green. This indicates that 
#they are meaningful for the regression

model <- lm(energy ~ toxic + green, data = states)
summary(model)
#Using this model, the accuracy went up to 75% which proves that the variable toxic and green
#are meaningful and relevant to the regression

ggplot(states, aes(x = toxic + green, y = energy)) +
  geom_point() + 
  stat_smooth(method = lm)
#Observing the plot, we can see that the regression is more identifiable than
#when the only predictor is the variable metro
