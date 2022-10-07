library(tidyverse)
library(ggplot2)

data("PlantGrowth")
head(PlantGrowth)

x <- "1" 
y <- rnorm(100) 
qplot(x, y, geom="boxplot")

qplot(group, weight, data = PlantGrowth, geom=c("boxplot"))

qplot(group, weight, data = PlantGrowth, geom=c("dotplot"), stackdir = "center", binaxis = "y") 

qplot(group, weight, data = PlantGrowth, geom=c("violin"), trim = FALSE)

qplot(group, weight, data = PlantGrowth, geom=c("boxplot", "jitter"), fill = group)

qplot(group, weight, data = PlantGrowth, geom = "dotplot", stackdir = "center", binaxis = "y", color = group, fill = group)

set.seed(1234)
mydata = data.frame( sex = factor(rep(c("F", "M"), each=200)), weight = c(rnorm(200, 55), rnorm(200, 58)))
head(mydata)

qplot(weight, data = mydata, geom = "histogram")

qplot(weight, data = mydata, geom = "histogram", fill = sex)

qplot(weight, data = mydata, geom = "density") 

qplot(weight, data = mydata, geom = "density", color = sex, linetype = sex)

qplot(weight, data = mydata, geom = "density", xlab = "Weight (kg)", ylab = "Density", main = "Density plot of Weight")

