library(arules)

groceries <- data("Groceries")
class(Groceries)
summary(Groceries)

frequentItems <- eclat (Groceries, parameter = list(supp = 0.07, maxlen = 15)) 

inspect(frequentItems)

itemFrequencyPlot(Groceries, topN=10, type="absolute", main="Item Frequency") 

rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.5)) 

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf))

rules_lift <- sort (rules, by="lift", decreasing=TRUE) 
inspect(head(rules_lift)) 

rules <- apriori(Groceries, parameter = list (supp = 0.001, conf = 0.5, maxlen=3)) 
subsetRules <- which(colSums(is.subset(rules, rules)) > 1) 
length(subsetRules) 
rules <- rules[-subsetRules]

rules <- apriori (data=Groceries, parameter=list
                  (supp=0.001,conf = 0.08), appearance = list
                  (default="lhs",rhs="whole milk"), control = list (verbose=F))

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf))

rules <- apriori (data=Groceries, parameter=list
                  (supp=0.001,conf = 0.15,minlen=2), appearance = list(default="rhs",lhs="whole milk"), 
                  control = list(verbose=F)) 

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf))

