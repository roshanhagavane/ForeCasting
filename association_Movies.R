install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)


mymovies <- read.transactions("D:/Study Material/DataScience/Association Rules/my_movies.csv")
inspect(mymovies[1:10])
class(mymovies)
movies_rules<-apriori(mymovies,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
inspect(movies_rules[1:10])
plot(movies_rules)







