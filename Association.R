# load the file
myread <- read.csv(file.choose())
View(myread)
attach(myread)
# imputing or omiting nas
sum(is.na(myread))

dim(myread)
sum(is.na(myread$ChildBks))
sum(is.na(myread$YouthBks))
sum(is.na(myread$CookBks))
sum(is.na(myread$DoItYBks))
sum(is.na(myread$RefBks))
sum(is.na(myread$ArtBks))
# here is no na value i found
# try to plot boxplot for finding an outliers
boxplot(myread$YouthBks)$out # here is outlier
boxplot(myread$ChildBks)$out #no
boxplot(myread$CookBks)$out  #no
boxplot(myread$ArtBks)$out # here is a outlier
boxplot(myread$GeogBks)$out # no
boxplot(myread$ItalCook)$out #here is 
summary(myread)
# importing arules and arulesviz & try to with apiori algo
library(arules)
install.packages("arulesViz")
library(arulesViz)
# i give minlen 3 and support is 2 % and confidence is 5%
books_rules <- apriori(as.matrix(myread),parameter = list(support = 0.02,confidence = 0.05,minlen =3))
books_rules # they give with that now i change and see
# set of 823 rules

books_rules <- apriori(as.matrix(myread),parameter = list(support = 0.05,confidence = 0.5,minlen =3))
books_rules # they give with that now i change and see
# set of 190 rules

books_rules <- apriori(as.matrix(myread),parameter = list(support = 0.07,confidence = 0.5,minlen =3))
books_rules # they give with that now i change and see
# set of  95 rules

books_rules <- apriori(as.matrix(myread),parameter = list(support = 0.09,confidence = 0.5,minlen =3))
books_rules # they give with that now i change and see
# set of 47 rules

# Now condition give me 47 rules




inspect(head(sort(books_rules,by ="lift")))   
class(myread)
# Now try to plot
plot(books_rules,method = "scatterplot")
plot(books_rules,method = "grouped")
plot(books_rules,method = "graph")

# all plots give me result on the basis of 47 rules

