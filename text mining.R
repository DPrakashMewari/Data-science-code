# IMDB movie review Extraction
# ----------***----------------

#import library which we need 
library(rvest)
library(xml2)
library(magrittr)

library(wordcloud)
library(tm)
library(dplyr)

 a<-10
sd<-NULL   # shaushank redemtion 
 url1<-"https://www.imdb.com/title/tt0111161/reviews?sort=helpfulnessScore&dir=desc&ratingFilter=0"
 for(i in 0:22){
   url<-read_html(as.character(paste(url1,i*a,sep="")))
   wd<-url %>%
     html_nodes(".lister-item-content") %>%
     html_text() 
     sd<-c(sd,wd)
 }
write.table(sd,file="shaushankredemtion1.txt",row.names = F)
dim(sd)

#import text file
View(review)
class(review)
length(review)
summary(review)

library(tm)
corpus <- review[-1,]
head(corpus)
class(corpus)

corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)
#remove punctuation
corpus <- tm_map(corpus,removePunctuation)
#remove number
corpus <- tm_map(corpus,removeNumbers)
#remove whitespaces
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))

inspect(cleanset[1:10])
# remove some words particularly
cleanset<-tm_map(cleanset,removeWords, c('\n','sign'))

# Removing the word movie and movies on similar grounds - as unnecessary.


cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')

inspect(cleanset[1:5])
# 
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm

tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# bar plot

w <- rowSums(tdm)
w <- subset(w,w>=50)
barplot(w, las = 2, col = rainbow(10))


# Word Cloud 

library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)


#load some library
# Extract moview review data
# Convert into corpus 
# clean text with uppercase lowercase stemming ...
# make tdm matrix
# Plot a bar plot to check frequency in it
# And then PLOT A wordcloud

