#  1.
# Load the file
crm <- read.csv(file.choose())
View(crm)
colnames(crm)

#we try to normalise a data 
normalized_data<-scale(crm[,2:4])
normalized_data                       
# after a normalise we calculate a distance euclidean
d <- dist(normalized_data, method = "euclidean") 
d
#complete method apply here 
fit <- hclust(d, method="complete")
# now to try to plot a dendogram
plot(fit,hang = -1)
# now we try to cut
rect.hclust(fit, k=3, border="red") # three cluster
# make a group now
groups <- cutree(fit, k=3) 

membership<-as.matrix(groups)
final <- data.frame(crm, membership)
View(final)

aggregate(crm[,-1],by=list(final$membership),mean)

#average method   [these methods changes were seeing drastically] 
fit2 <- hclust(d,method = "average")
plot(fit2,hang = -1)

# now we try to cut
rect.hclust(fit2, k=4, border="red") # try with 4 cluster

# make a group now
groups1<- cutree(fit2, k=4) 
membership1<-as.matrix(groups1)
final1 <- data.frame(crm,membership1)
View(final1)
fit2 <- hclust(d,method = "average")
plot(fit2,hang = -1)

# now we try to cut
rect.hclust(fit2, k=4, border="red") # try with 4 cluster

# make a group now
groups1<- cutree(fit2, k=4) 
membership1<-as.matrix(groups1)
final1 <- data.frame(crm,membership1)
View(final1)

aggregate(crm[,-1],by=list(final1$membership1),mean)

aggregate(crm[,-1],by=list(final1$membership1),mean)
# they are aggregating according to there group by Murder,urbanpop,rape

?dist
# let try with different method
d1 <- dist(normalized_data, method = "manhattan") 
d1

#complete method apply here 
fit3 <- hclust(d, method="single")
# now to try to plot a dendogram
plot(fit3,hang = -1)
# now we try to cut
rect.hclust(fit3, k=3, border="red") # three cluster
# make a group now
groups3 <- cutree(fit3, k=3) 

membership2<-as.matrix(groups3)
final2 <- data.frame(crm, membership2)
View(final2)

aggregate(crm[,-1],by=list(final2$membership2),mean)

#----------------*---------------
  
# Here we do load file and Normalise data to 0,1
# Apply method in our Normalised data to find a distance
# Then we apply some method of a clustering (Hierarchial clustering)on our
# fitted data then check with plot 
# making a group assingn those cluster in a group



