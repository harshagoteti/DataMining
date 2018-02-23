################## Setting up Facebook API #######################
install.packages("devtools")  ## from CRAN
install.packages("Rfacebook") #http://thinktostart.com/analyzing-facebook-with-r/# from CRAN
install.packages("Rook")
library(devtools)
install_github("pablobarbera/Rfacebook/Rfacebook")  # from GitHub
## NOTE: I had to install this library manually by download the zipfile from GitHub and extracting to the R libraries directory

## There are two different ways of making authenticated requests:
## 1. Get a temporary (2 hr) access token from https://developers.facebook.com/tools/explorer/, which can be used as
## argument in any of the functions in Rfacebook. 
## 2.  Get a permanent OAuth token
## a. Go to https://developers.facebook.com/apps, register as a developer and create a new
## app. You will also need a verified Facebook account. After that, click in "Show" under "App Secret"
## to find your 'App ID' and 'App Secret'.
## b. In R, install the latest version of devtools and of RFacebook from GitHub.
## c.  Connect R session with FB test app and authenticate it to our Facebook Profile.
## Rfacebook offers a very easy function for that.
## Run the fbOAuth function with your "App ID" and "App Secret" as arguments. It will
## return a URL, which you will need to paste into the "Website with Facebook login" field in your
## App Settings on Facebook. Once you've done so, press Enter.
## Third, after pressing Enter, R will try to open a browser window to sign the token. If everything
## works well, you will get a message that says you can return to R. If not, try again in a few minutes
## to make sure your app had its settings updated properly.
## To ensure proper functioning of the "getInsights" function-family you will need to specify the exact
## permissions granted to your app. As this is (to our knowledge) currently not possible through the R
## based authentication process, please follow these steps:
##   -> Create App as mentioned above. 1. Open the "Graph API Explorer": https://developers.
## facebook.com/tools/explorer/ 2. Select your app in the upper right corner 3. Click "Get Token"
## -> "Get Access Token" 4. In the popup navigate to "Extended Permissions" and select "Insights"
## 5. Confirm 6. Ignore the following warning message ("Submit for Login Review...") and confirm
## again. 6. Go back to R and run fbOAuth with extended_permissions (still) set to FALSE. -> See
## third step for possible messages concerning token creation.

require("Rfacebook")

fb_appid <- '<your appid here>'
fb_appsec <- '<your appsec here>'
fbOAuth(fb_appid,fb_appsec, extended_permissions = TRUE, legacy_permissions = FALSE)

fb_oauth <- fbOAuth(fb_appid,fb_appsec, extended_permissions = TRUE, legacy_permissions = FALSE)
save(fb_oauth, file="C:\Program Files\R\fb_oauth")
load("fb_oauth")

################## Let's look around Facebook! ########################

## Saving my own public information in the variable "me".
me <- getUsers("me",token=fb_oauth)
me
my_likes <- getLikes(user="me", token=fb_oauth)
my_likes
my_friends <- getFriends(token, simplify = FALSE)
my_friends

################## The remainder didn't work for me with fb_oauth
################## Generated temporary token at https://developers.facebook.com/tools/explorer

library(Rfacebook)

token <- "<your token here"
me <- getUsers("<yourname>", token, private_info = TRUE)
me$name # my name

## my_newsfeed <- getNewsfeed(token=fb_oauth, n=100) ## need to debug; throws API permission error
my_newsfeed <- getNewsfeed(token, n=100) ## 
post <- getPost(post_id,token=token, n = 1000, likes = TRUE, comments = TRUE)  ## need to debug; graph API error

