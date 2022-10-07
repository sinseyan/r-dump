library(ggplot2)

dir.create("plots")

data(mtcars) 
df <- mtcars[, c("mpg", "cyl", "wt")] 
head(df)

x <- 1:10; y = x*x 
qplot(x,y) 

qplot(x, y, geom=c("point", "line")) 

qplot(mpg, wt, data=mtcars)

qplot(mpg, wt, data = mtcars, geom = c("point", "smooth"))

qplot(mpg, wt, data = mtcars, color = factor(cyl), geom=c("point", "smooth"))

qplot(mpg, wt, data = mtcars, colour = cyl) 

df <- mtcars 
df[,'cyl'] <- as.factor(df[,'cyl']) 
qplot(mpg, wt, data = df, colour = cyl) 

qplot(mpg, wt, data = df, colour = cyl, geom=c("point", "line"))

qplot(mpg, wt, data=df, colour= factor(cyl))

qplot(mpg, wt, data = mtcars, size = mpg) 

qplot(mpg, wt, data = mtcars, shape = factor(cyl))

qplot(mpg, wt, data = mtcars, label = rownames(mtcars), geom=c("point", "text"), hjust=0, vjust=0)

