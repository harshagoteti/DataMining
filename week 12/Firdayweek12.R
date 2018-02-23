install.packages("devtools")
install.packages("Rfacebook")
install.packages("Rook")
library(devtools)
install_github("pablobarbera/Rfacebook/Rfacebook")
require("Rfacebook")


fb_appid <- '1675043216133450'
fb_appsec <- '5407d86646a2525720beec8f42affb5c'
fbOAuth(fb_appid,fb_appsec, extended_permissions = TRUE, legacy_permissions = FALSE)

fb_oauth <- fbOAuth(fb_appid,fb_appsec, extended_permissions = TRUE, legacy_permissions = FALSE)
save(fb_oauth, file="~/R/fb_oauth")
load("~/R/fb_oauth")

################## Let's look around Facebook! ########################

## Saving my own public information in the variable "me".
library(V8)
library(RCurl)
library(RJSONIO)
library(rjson)
me <- getUsers("Sriharsha Goteti",token=fb_oauth)
me
my_likes <- getLikes(user="Sriharsha Goteti", token=fb_oauth)
my_likes
my_friends <- getFriends(token, simplify = FALSE)
my_friends

################## The remainder didn't work for me with fb_oauth
################## Generated temporary token at https://developers.facebook.com/tools/explorer

library(Rfacebook)

token <- "EAACEdEose0cBAJPrmpkctu5ZAZBNSHX3t3CtaX8ivaEEH5ZA6R1LQQSiyWBEVzQTKg1876iT2ePlcLCcWLBNIVQFTTykYH4ZC4VWt3sdmYvRcxaloZBjogYqPSEZAXvmNhswRSav7jqA7YuZCAUIMvw0ZADhMZCW00zVsAK03mi3KrDjegp2ilrM0"
me <- getUsers("me", token, private_info = TRUE)
me$name # my name

## my_newsfeed <- getNewsfeed(token=fb_oauth, n=100) ## need to debug; throws API permission error
my_newsfeed <- getNewsfeed(token, n=100) ## 
post <- getPost(10205469233992080,token=token, n = 1000, likes = TRUE, comments = TRUE)
str(me)
str(my_newsfeed)
library(tm)
myCorpus <- Corpus(VectorSource(my_newsfeed))

library(wordcloud)
m <- as.matrix(me)
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
myCorpus <- tm_map(myCorpus, stemDocument)
## inspect documents (tweets) numbered 11 to 15
inspect(myCorpus[11:15])
myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpus <- Corpus(VectorSource(myCorpus))
myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
## myTdm <- TermDocumentMatrix(myCorpus)
myTdm
idx <- which(dimnames(myTdm)$Terms == "r")
inspect(myTdm[idx+(0:5),101:110])
layout(matrix(c(1,2),2,1)) # set to two graphs per page
plot(pamResult, color=F, labels=4, lines=0, cex=.8, col.clus=1, col.p=pamResult$clustering)
layout(matrix(1)) # change back to one graph per page
library(wordcloud)
 m <- as.matrix(myTdm)
 wordFreq <- sort(rowSums(m), decreasing=TRUE)
 set.seed(375) # to make it reproducible
 grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
 wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F, colors=grayLevels)
 
 ##Network Graph####
 
 ## load termDocMatrix
 termDocMatrix <- wordFreq
 ## inspect part of the matrix
 termDocMatrix[1:10,1:20]
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
 library(fpc)
 ## build a graph from the above matrix
 g <- graph.adjacency(termMatrix, weighted=T, mode="undirected")
 
 
 ***Graph Modulation****
   
   
   V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
 V(g)$label.color <- rgb(0, 0, .2, .8)
 V(g)$frame.color <- NA
 egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
 E(g)$color <- rgb(.5, .5, 0, egam)
 E(g)$width <- egam
 ## plot the graph in layout1
 plot(g, layout=layout1)
 plot(g)
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
 
 