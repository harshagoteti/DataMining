############## tweepy documentation ###################
## http://docs.tweepy.org/en/v3.5.0/index.html ########

from __future__ import print_function ## This is to print to a log
import sys ## Always a good idea
import tweepy
from tweepy import OAuthHandler  ## This is the Twitter REST connection API
from tweepy import Stream  ## This is the Streaming API
from tweepy.streaming import StreamListener  ## This library will listen to the Twitter stream


## Consumer key, consumer secret, access token, access secret.
ckey="gfEeOcZBTc7opkA9kfOKFNqMj"
csecret="wk0HX9bmZrSL8IYLG9fKVOJbqQ7TsT4zrdA0GK5mjEPo9HGGqM"
atoken="140462594-T8cwsEWSrO3Xq7pxTwrZCli5Rxe0ieeBYxl6LaOE"
asecret="lrNhamRnMPP9yZ9GKq8ayfdIsSe6uPz1WR1RVvKrDxQqN"

auth = OAuthHandler(ckey, csecret)
auth.set_access_token(atoken, asecret)

api = tweepy.API(auth)

#print ("API NAME IS: ", api.me().name)

############## USING THE REST API ######################
############## Pulling my own timeline #################

#user = api.me()
#print (user)

##### Printing screen output to file #####

sys.stdout=open("C:/DM SEM 2/week 11/script.txt","w")
#print (user)
sys.stdout.close()

##print 'MY TWEETS'
##my_tweets = api.home_timeline()
##for tweet in my_tweets:
##print tweet.text

################ Displaying user information ##########

##user1 = api.get_user('Srinivasa Goteti')
  ##print 'SCREEN NAME:'
 ##print user1.screen_name
 ##print 'NUMBER OF FOLLOWERS:'
##print user1.followers_count
 ##for follower in user1.followers():
   ##  print follower.screen_name
## print 'USER FRIENDS:'
 ##for friend in user1.friends():
   ##  print friend.screen_name

############## Pulling another user's timeline #################

##print''
##print '##### USER TWEETS #####'
##user_tweets = api.user_timeline(id='wired',count='5')
##for tweet in user_tweets:
  ##  print tweet.text

############## Pulling in a hashtag ############################

##print ''
 ##print '##### HASHTAG SEARCH #####'
search_text = "#roommate"
search_number = 200
search_result = api.search(search_text, rpp=search_number)
for i in search_result:
    print (i.text)

############## USING THE STREAMING API #################
############### Pulling in a hashtag ###################
### This will not stop until Twitter closes the connection ###########

## print''
## print'##### STREAM #####'

## class listener(StreamListener):

##     def on_data(self, data):
##         print(data)
##         return(True)

##     def on_error(self, status):
##         print status

## twitterStream = Stream(auth, listener())
## twitterStream.filter(track=["Chicago"])


################ 

