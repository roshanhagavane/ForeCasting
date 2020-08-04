install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)


groceries<-read.transactions("D:/Study Material/DataScience/Association Rules/groceries.csv",format="basket")
inspect(groceries[1:10])
class(groceries)

groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
inspect(groceries_rules[1:10])
plot(groceries_rules)

class(groceries)






