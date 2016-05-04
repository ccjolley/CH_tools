# Get a list of twitter profiles based on CH output

library(XLConnect)
library(twitteR)
library(plyr)

###############################################################################
# Load CH output data
###############################################################################
get_ws <- function(dir,patterns) {
  setwd(dir)
  fnames <- unlist(sapply(patterns, function(x) list.files(pattern=x)))
  ldply(fnames, function(x) readWorksheetFromFile(x,sheet=1))
}

ws <- get_ws(dir="C:/Users/Craig/Dropbox/SMA/Exchange",
       c('^India.*xls','^Kenya.*xls'))

###############################################################################
# Set up Twitter API
# twitteR documentation at: http://cran.r-project.org/web/packages/twitteR/twitteR.pdf
# Codes from apps.twitter.com/app/8190625
###############################################################################
key <- 'aZolKBHmBUotTNpf5p1gk5SJ3'
secret <- 'S8wSc8KNImg12Ho1ydkzM7HkUX3GRx13JkFipWli4Bvw0mP8ER'
access <- '862564638-2vhtPUVAR5NVHzduKp9396jw1ipToSkT1k7KoFdQ'
access_secret <- '1wH5xEwB6Y7Pr5Ta6GNegFuDU6HOzIq17KMB0q3ib0e9Z'
setup_twitter_oauth(key,secret,access,access_secret)

###############################################################################
# Pull info from twitter API
###############################################################################
get_profiles <- function(s) {
  # strip '@', if present
  names <- sub('^@','',s)
  users <- lookupUsers(names)
  ldply(users,function(x) 
    data.frame(screenName=x$screenName,name=x$name,description=x$description))
}

test <- ws$Author[1:5]
profiles <- get_profiles(test)
  