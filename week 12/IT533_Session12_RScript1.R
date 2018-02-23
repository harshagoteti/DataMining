## This is a review of Twitter text retrieval and data mining methods (esp. clusters)
## Zhao, Chapter 10

library(twitteR)
library(wordcloud)
library(tm)
library(qdap)
library(SnowballC)

##### Setting up Twitter connection #####

## consumer_key <- 'your_consumer_key'
## consumer_secret <- 'your_consumer_secret'
## access_token <- 'your_access_token'
## access_secret <- 'your_access_secret'

## setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

consumer_key <- 'szoaFeVByen8dvRuonH7r1xaE'
consumer_secret <- 'kN8L5uf7tFY1orC8J1yC6yyEBctolR64MgO2z8qZp23YZ3LUDl'
access_token <- '472437625-2h9R2VfjahADRYPgnKLWnmcWnNe5M3RmZvk84dMC'
access_secret <- 'h9mlKxSgc8vZprwDkw5anMORpG1HpTDFuCLZnjnTnRABT'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#### Get the first 200 tweets (or all tweets if fewer than 200) from the user timeline of @rdatammining #####
rdmTweets <- userTimeline("rdatamining", n=200)
(nDocs <- length(rdmTweets))

## Next, we have a look at the five tweets numbered 11 to 15.
rdmTweets[11:15]

##### Transforming Text #####

## convert tweets to a data frame
df <- do.call("rbind", lapply(my_newsfeed, as.data.frame))
dim(df)

## build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(df$`X[[i]]`))

## convert to lower case
myCorpus <- tm_map(myCorpus, tolower)
## remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
## remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
## remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
## add two extra stop words: "available" and "via"
myStopwords <- c(stopwords('english'), "available", "via")
## remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
## remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
## keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
## stem words
myCorpus <- tm_map(myCorpus, stemDocument)
## inspect documents (tweets) numbered 11 to 15
inspect(myCorpus[11:15])

##### Building a Term-Document Matrix #####

## each row stands for a term and each column for a document, and an entry is the number of 
## occurrences of the term in the document.  We use  function TermDocumentMatrix(), but. With 
## its default setting, terms with less than three characters are discarded. To keep \r" in 
## the matrix, we set the range of wordLengths in the example below

## The following line is required because tm_map returns character vector instead of PlainTextDocument.
myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpus <- Corpus(VectorSource(myCorpus))
myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
## myTdm <- TermDocumentMatrix(myCorpus)
myTdm

## The TDM is very sparse, with 99% of the entries being zero. We then look at the
## first six terms starting with \r" and tweets numbered 101 to 110.

idx <- which(dimnames(myTdm)$Terms == "r")
inspect(myTdm[idx+(0:5),101:110])

####################### NOW WE USE ANALYTICS #######################

##### Frequent Terms and Associations #####
## We look at the popular words and the association between words. 
3# inspect frequent words.  findFreqTerms() finds frequent terms with frequency no less than ten.
findFreqTerms(myTdm, lowfreq=10)

## To show the top frequent words visually, we next make a barplot for them. From the termdocument
## matrix, we can derive the frequency of terms with rowSums(). Then we select terms that
## appear in ten or more documents and shown them with a barplot using package ggplot2

termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=10)

library(ggplot2)
##qplot(names(termFrequency), termFrequency, geom="bar", xlab="Terms") + coord_flip() 
barplot(termFrequency, las=2)



##### Associated Words #####
## Using function findAssocs(), we search for terms associated with \r" (or \mining") 
## with correlation no less than 0.25, and the words are ordered by their correlation with \r" (or \mining").
## which words are associated with "r"?
findAssocs(myTdm, 'r', 0.25)
## which words are associated with "mining"?
findAssocs(myTdm, 'mining', 0.25)

##### Wordcloud #####
library(wordcloud)
m <- as.matrix(myTdm)
## calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)

## word cloud
set.seed(375) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F, colors=grayLevels)

###### Clustering Words #####
## We search for clusters of words with hierarchical clustering. 
## Sparse terms are removed, so that the plot of clustering will not be crowded with words. 
## Then the distances between terms are calculated with dist() after scaling. 
## After that, the terms are clustered with hclust() and the dendrogram is cut into 10 clusters. 
## The agglomeration method is set to ward, which denotes the increase in variance when 
## two clusters are merged. Some other options are single linkage, complete linkage, average linkage, median and centroid.
## remove sparse terms
myTdm2 <- removeSparseTerms(myTdm, sparse=0.95)
m2 <- as.matrix(myTdm2)

##### Hierarchical Cluster #####
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward.D")
plot(fit)
## cut tree into 10 clusters
rect.hclust(fit, k=10)
(groups <- cutree(fit, k=10))

##### kMeans Cluster #####
## k-means clustering takes the values in the matrix as numeric. We transpose
## the term-document matrix to document-term> The tweets are then clustered with kmeans()
## with the number of clusters set to eight. After that, we check the popular words in every cluster
## and also the cluster centers. Note that a fixed random seed is set with set.seed() before running
## kmeans(), so that the clustering result can be reproduced.
## transpose the matrix to cluster documents (tweets)
m3 <- m2
## set a fixed random seed
set.seed(122)
## k-means clustering of tweets
k <- 8

kmeansResult <- kmeans(m3, k)
## cluster centers
round(kmeansResult$centers, digits=3)

## To see what the clusters are about, we review the top three words in each cluster.
for (i in 1:k) {
cat(paste("cluster ", i, ": ", sep=""))
s <- sort(kmeansResult$centers[i,], decreasing=T)
cat(names(s)[1:3], "\n")
# print the tweets of every cluster
# print(rdmTweets[which(kmeansResult$cluster==i)])
}

## From the  top words and centers of clusters, we can see that the clusters focus on different topics!

##### k-medoids Algorithm #####
## We then use k-medoids clustering with the Partitioning Around Medoids (PAM) algorithm, 
## which uses medoids (representative objects) instead of means to represent clusters. 
## It is more robust to noise and outliers than k-means clustering, and provides a display 
## of the silhouette plot to show the quality of clustering.

library(fpc)
## partitioning around medoids with estimation of number of clusters
pamResult <- pamk(m3, metric="manhattan")
## number of clusters identified
(k <- pamResult$nc)
pamResult <- pamResult$pamobject
## print cluster medoids
for (i in 1:k) {
cat(paste("cluster", i, ": "))
cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
# print tweets in cluster i
# print(rdmTweets[pamResult$clustering==i])
}
## plot clustering result
layout(matrix(c(1,2),2,1)) # set to two graphs per page
plot(pamResult, color=F, labels=4, lines=0, cex=.8, col.clus=1, col.p=pamResult$clustering)
layout(matrix(1)) # change back to one graph per page

## With the silhouette, a large si (almost 1) suggests that the corresponding observations 
## are very well clustered, a small si (around 0) means that the observation lies between 
## two clusters, and observations with a negative si are probably placed in the wrong cluster. 

## To improve the clustering quality, we can set the range of cluster numbers
## krange=2:8 when calling pamk(), and in the new clustering result, there are eight clusters, with
## the observations in the above cluster 8 assigned to other clusters,
pamResult2 <- pamk(m3, krange=2:8, metric="manhattan")

###########################################################################################
###########################################################################################
############## SOCIAL NETWORK ANALYSIS ####################################################

## We first build a network of terms based on their co-occurrence in the same
## tweets, and then build a network of tweets based on the terms shared by them. At last, we
## build a two-mode network composed of both terms and tweets.

##### Network of Terms #####
## First, we  build a network of terms based on their co-occurrence in tweets. At first,
## a term-document matrix, termDocMatrix, is loaded into R, which is actually a copy of m2 (see above)). 
## After that, it is transformed into a term-term adjacency matrix, based on which a graph is built. 
## Then we plot the graph to show the relationship between frequent terms, and also make the graph 
## more readable by setting colors, font sizes and transparency of vertices and edges.

## Putting it in a general scenario of social networks, the terms can be taken as people and
## the tweets as groups on, for example, LinkedIn, and the term-document matrix can then be taken as the group
## membership of people.
## NOTE: %*% is an operator for the product of two matrices, and t() transposes a matrix

## load termDocMatrix
termDocMatrix <- m2
## inspect part of the matrix
termDocMatrix[5:10,1:20]
## change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1
## transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)
## inspect terms numbered 5 to 10
termMatrix[5:10,5:10]

## Now we have built a term-term adjacency matrix, where the rows and columns represent
## terms, and every entry is the number of concurrences of two terms. Next we can build a graph
## with graph.adjacency() from package igraph

library(igraph)
## build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode="undirected")
## remove loops
g <- simplify(g)
## set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

## set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g) ## this is a graph named after its inventors
plot(g, layout=layout1)

## A different layout can be generated with the first line of code below. The second line produces
## an interactive plot, which allows us to manually rearrange the layout.
plot(g, layout=layout.kamada.kawai)
tkplot(g, layout=layout.kamada.kawai)

## We can also save the network graph into a .PDF file with the code below.
pdf("term-network.pdf")
plot(g, layout=layout.fruchterman.reingold)
dev.off()

## Next, we set the label size of vertices based on their degrees, to make important terms stand
##out. Similarly, we also set the width and transparency of edges based on their weights. This is
##useful in applications where graphs are crowded with many vertices and edges. In the code below,
##the vertices and edges are accessed with V() and E(). Function rgb(red, green, blue, alpha)
## defines a color, with an alpha transparency. With the same layout before, we plot the
## graph again

V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
## plot the graph in layout1
plot(g, layout=layout1)

##### Network of Tweets #####
## We can also build a graph of tweets base on the number of terms that they have in common. 
## Because most tweets contain one or more words from \r", \data" and \mining", most tweets 
## are connected with others and the graph of tweets is very crowded. To simplify the graph 
## and find relationships between tweets beyond the above three keywords, we remove the three words before building a graph.

## remove "r", "data" and "mining"
idx <- which(dimnames(termDocMatrix)$Terms %in% c("r", "data", "mining"))
M <- termDocMatrix[-idx,]
## build a tweet-tweet adjacency matrix
tweetMatrix <- t(M) %*% M
library(igraph)
g <- graph.adjacency(tweetMatrix, weighted=T, mode = "undirected")
V(g)$degree <- degree(g)
g <- simplify(g)
## set labels of vertices to tweet IDs
V(g)$label <- V(g)$name
V(g)$label.cex <- 1
V(g)$label.color <- rgb(.4, 0, 0, .7)
V(g)$size <- 2
V(g)$frame.color <- NA
pdf("tweet-network.pdf")
barplot(table(V(g)$degree))
dev.off()

## With the code below, we set vertex colors based on degree, and set labels of isolated vertices to
## tweet IDs and the first 20 characters of every tweet. The labels of other vertices are set to tweet
## IDs only, so that the graph will not be overcrowded with labels. We also set the color and width
## of edges based on their weights. 
idx <- V(g)$degree == 0
V(g)$label.color[idx] <- rgb(0, 0, .3, .7)
## load twitter text
library(twitteR)
rdmTweets  ## from above
## convert tweets to a data frame
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
## set labels to the IDs and the first 20 characters of tweets
V(g)$label[idx] <- paste(V(g)$name[idx], substr(df$text[idx], 1, 20), sep=": ")
egam <- (log(E(g)$weight)+.2) / max(log(E(g)$weight)+.2)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
set.seed(3152)
layout2 <- layout.fruchterman.reingold(g)
plot(g, layout=layout2)

## The vertices in crescent are isolated from all others, and next we remove them from graph with
## function delete.vertices() and re-plot the graph
## This will give us a Network of Tweets

g2 <- delete.vertices(g, V(g)[degree(g)==0])
plot(g2, layout=layout.fruchterman.reingold)

## We can also remove edges with low degrees to simplify the graph. Below with function
## delete.edges(), we remove edges which have weight of one. After removing edges, some vertices
## become isolated and are also removed

g3 <- delete.edges(g, E(g)[E(g)$weight <= 1])
g3 <- delete.vertices(g3, V(g3)[degree(g3) == 0])
plot(g3, layout=layout.fruchterman.reingold)

## There are some groups (or cliques) of tweets. Let's have a look at one of the groups
df$text[c(7,12,6,9,8,3,4)]  ## or insert any other node numbers that you want to inspect

##### Two-Mode Network #####
## A two-mode network is composed of two types of vertices: tweets and terms. 
## At first, we generate a graph g directly from termDocMatrix. After that, different
## colors and sizes are assigned to term vertices and tweet vertices. 
## We also set the width and color of edges. The graph is then plotted with 
## layout.fruchterman.reingold
## create a graph
g <- graph.incidence(termDocMatrix, mode=c("all"))
## get index for term vertices and tweet vertices
nTerms <- nrow(M)
nDocs <- ncol(M)
idx.terms <- 1:nTerms
idx.docs <- (nTerms+1):(nTerms+nDocs)
## set colors and sizes for vertices
V(g)$degree <- degree(g)
V(g)$color[idx.terms] <- rgb(0, 1, 0, .5)
V(g)$size[idx.terms] <- 6
V(g)$color[idx.docs] <- rgb(1, 0, 0, .4)
V(g)$size[idx.docs] <- 4
V(g)$frame.color <- NA
## set vertex labels and their colors and sizes
V(g)$label <- V(g)$name
V(g)$label.color <- rgb(0, 0, 0, 0.5)
V(g)$label.cex <- 1.4*V(g)$degree/max(V(g)$degree) + 1
## set edge width and color
E(g)$width <- .3
E(g)$color <- rgb(.5, .5, 0, .3)
set.seed(958)
plot(g, layout=layout.fruchterman.reingold)

## Now check how tweets are grouped around two centers, for example \r" and \data mining".
## Inspect the centers further
V(g)[nei("r")]  ## If one of the centers is \r"

## An alternative way is using function neighborhood() as below.
V(g)[neighborhood(g, order=1, "r")[[1]]]
## We can also have a further look at which tweets contain all three terms: \r", \data" and \mining".
(rdmVertices <- V(g)[nei("r") & nei("data") & nei("mining")])
df$text[as.numeric(rdmVertices$label)]

## Next, we remove \r", \data" and \mining" to show the relationship between tweets with other
## words. Isolated vertices are also deleted from graph.

idx <- which(V(g)$name %in% c("r", "data", "mining"))
g2 <- delete.vertices(g, V(g)[idx-1])
g2 <- delete.vertices(g2, V(g2)[degree(g2)==0])
set.seed(209)
plot(g2, layout=layout.fruchterman.reingold)

## Now we can clearly see groups of tweets and their keywords, such as time series,
## social network analysis, parallel computing and postdoctoral and research positions
