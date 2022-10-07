library(tree)
library(ISLR)
data <- Carseats
str(data)
glimpse(data)

Salecat <- ifelse(data$Sales<=8,"No","Yes")
data <- data.frame(data,Salecat)

class(data$Salecat)
data$Salecat = as.factor(data$Salecat)
class(data$Salecat)

sale.tree <- tree(Salecat~.-Sales,data = data)
summary(sale.tree)

plot(sale.tree)
text(sale.tree, pretty=0)

set.seed (2)
train = sample(1: nrow( Carseats ), 200)
Carseats.test = Carseats [- train ,]
Salecat.test = Salecat [- train ]
Salecat = as.factor(Salecat)
tree.carseats = tree( Salecat~.-Sales ,Carseats ,subset = train )
tree.pred= predict (tree.carseats , Carseats.test , type ="class")
table (tree.pred , Salecat.test )


set.seed (3)
cv.carseats =cv.tree(tree.carseats, FUN=prune.misclass )
names (cv.carseats )

par (mfrow =c(1 ,2))
plot(cv.carseats$size ,cv.carseats$dev ,type ="b")
plot(cv.carseats$k ,cv.carseats$dev ,type ="b")

prune.carseats = prune.misclass (tree.carseats ,best =9)
plot( prune.carseats )
text(prune.carseats , pretty =0)
print(prune.carseats)

tree.pred= predict (prune.carseats, Carseats.test ,type ="class")
table (tree.pred , Salecat.test )

