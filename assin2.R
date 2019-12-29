#2 Airlines data

# clustering
library(plyr)
input <- read.csv(file.choose())
View(input)
mydat1 <- input[-c(1)]
View(mydat1)
summary(mydat1)

# normalise or standarise
normalized_data <- scale(mydat1)
d <- dist(normalized_data,method = "euclidean")                 
fit <- hclust(d,method="complete") # depend upon u
str(fit) 


fit$order

plot(fit,hang=-1) # Distance 
#how mant cluster?
#which two record merged first
# Difference cuts to see how many cluster can be found

rect.hclust(fit,k=3,border ="red")


groups <- cutree(fit,k=3) # cut into three clusters
groups # all label clusrer

membership <- as.matrix(groups)

final <- data.frame(mydat1,membership)

View(final) # they all are in one group

# Now we K-Mean clustering
View(input)
mydata <- input[-c(1)]

# Normalise the data
normalized_data1 <- scale(mydata)

fit1 <- kmeans(normalized_data1,3) # try with three clusters
str(fit1)
# check the groups
fit1$cluster

# we also creater matrix but her we create a data frame
final2 <- data.frame(mydata,fit1$cluster)

#aggregating a data
aggregate(mydata,by=list(fit1$cluster),FUN = mean)

#plot to check centroid
library(animation)
windows()
km <- kmeans.ani(mydata,3)# check with 3

# check optimum for cluster
wss <- c()
for(i in 2:11)
   wss[i] <- sum(kmeans(mydata,centers = i)$withnss)
plot(1:11,wss,type="b",xlab="No of cluster",ylab ="Avg of distance") # No Elbow there
# No Elbow there but we can choose k value upto 10


#------------------------