################# Text Retrieval with Twitter ##################################################
## NOTE:  You will need your own Twitter account and a completed OAuth Profile.  See:
## https://twittercommunity.com/t/oauth-authentication-with-twitter-api-via-r-failed/390

install.packages("twitteR")
install.packages("wordcloud")
install.packages("tm")
install.packages("qdap")
library(twitteR)
library(wordcloud)
library(tm)
library(qdap)

## Potentially necessary file for Windows
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

consumer_key <- 'gfEeOcZBTc7opkA9kfOKFNqMj'
consumer_secret <- 'wk0HX9bmZrSL8IYLG9fKVOJbqQ7TsT4zrdA0GK5mjEPo9HGGqM'
access_token <- '140462594-T8cwsEWSrO3Xq7pxTwrZCli5Rxe0ieeBYxl6LaOE'
access_secret <- 'lrNhamRnMPP9yZ9GKq8ayfdIsSe6uPz1WR1RVvKrDxQqN'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


## Necessary file for Windows
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

################## Text Retrieval and Wordcloud ###############################
#Code (with some changes) from http://davetang.org/muse/2013/04/06/using-the-r_twitter-package/
#download.file(url="http://davetang.org/muse/2013/04/06/using-the-r_twitter-package/", destfile = "cacert.pem")
###############################################################################

library(twitteR)

## Looks like the cainfo is no longer needed 
DonaldTrump <- searchTwitter("#DonaldTrump", n=500)
mrm <- searchTwitter("#roommate", n=500)
## should get 1500
length(DonaldTrump)
length(mrm)
## [1] 1500
str(DonaldTrump [1:1])
str(mrm [1:1])
## This is what the Twitter attributes look like.  Awesome, huh?

## Save text
DonaldTrump_text <- sapply(DonaldTrump, function(x) x$getText())
mrm_text <- sapply(mrm, function(x) x$getText())
## Create corpus
mrm_text_corpus <- Corpus(VectorSource(mrm_text))
DonaldTrump_text_corpus <- Corpus(VectorSource(DonaldTrump_text))

## Clean up

## Transform special characters to readable latin1
mrm_text_corpus$text <- sapply(mrm_text_corpus$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
DonaldTrump_text_corpus$text <- sapply(DonaldTrump_text_corpus$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
## Make all lowercase
DonaldTrump_text_corpus <- tm_map(DonaldTrump_text_corpus, content_transformer(tolower)) 
mrm_text_corpus <- tm_map(mrm_text_corpus, content_transformer(tolower)) 
## Remove punctuation
DonaldTrump_text_corpus <- tm_map(DonaldTrump_text_corpus, removePunctuation)
mrm_text_corpus <- tm_map(mrm_text_corpus, removePunctuation)
## Remove stopwords
DonaldTrump_text_corpus <- tm_map(DonaldTrump_text_corpus, function(x)removeWords(x,stopwords()))
mrm_text_corpus <- tm_map(mrm_text_corpus, function(x)removeWords(x,stopwords()))
## Tadah!  Build the wordcloud and ignore the errors!
wordcloud(DonaldTrump_text_corpus)
wordcloud(mrm_text_corpus)

## Alternative steps if you're running into problems 
## cats<- searchTwitter("#Rstats", n=1500, cainfo="cacert.pem")

## Save text
## cats_text <- sapply(cats, function(x) x$getText())
## create corpus
## cats_text_corpus <- Corpus(VectorSource(cats_text))

## if you get the below error
## In mclapply(content(x), FUN, ...) :
## all scheduled cores encountered errors in user code
## add mc.cores=1 into each function

## run this step if you get the error:
## (please break it!)' in 'utf8towcs'
## cats_text_corpus <- tm_map(cats_text_corpus,
##                               content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
##                               mc.cores=1
## )
## cats_text_corpus <- tm_map(cats_text_corpus, content_transformer(tolower), mc.cores=1)
## cats_text_corpus <- tm_map(cats_text_corpus, removePunctuation, mc.cores=1)
## cats_text_corpus <- tm_map(cats_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
## wordcloud(cats_text_corpus)

mrm2 = userTimeline("realDonaldTrump",n=1000)
tw=twListToDF(mrm2)
vec=tw$text
extract.hashes = function(vec){
  
  ##  hash.pattern = "#[[:alpha:]]+"
  hash.pattern = "#[[:alnum:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  
  hash.matches = gregexpr(pattern = hash.pattern,
                          text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
  
  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("tag","freq")
  df = df[order(df$freq,decreasing = TRUE),]
  return(df)
}

dat = head(extract.hashes(vec),50)

dat2 = transform(dat,tag = reorder(tag,freq))

library(ggplot2)

p = ggplot(dat2, aes(x = tag, y = freq)) + geom_bar(stat = "identity", fill = "blue")
p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the Obama team (@BarackObama)")


