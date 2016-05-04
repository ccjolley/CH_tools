library(twitteR)

twitter_info <- function(s) {
  # Given a list of twitter 
  # handles, return a data frame with the handle, user name, 
  # profile text, and location. Omit '@' at beginning of names.
  
  # twitteR documentation at: http://cran.r-project.org/web/packages/twitteR/twitteR.pdf
  # Codes from apps.twitter.com/app/8190625
  key <- 'aZolKBHmBUotTNpf5p1gk5SJ3'
  secret <- 'S8wSc8KNImg12Ho1ydkzM7HkUX3GRx13JkFipWli4Bvw0mP8ER'
  access <- '862564638-2vhtPUVAR5NVHzduKp9396jw1ipToSkT1k7KoFdQ'
  access_secret <- '1wH5xEwB6Y7Pr5Ta6GNegFuDU6HOzIq17KMB0q3ib0e9Z'
  setup_twitter_oauth(key,secret,access,access_secret)
  
  users <- lookupUsers(strsplit(s,' ')[[1]])
  data.frame(name=sapply(users,function(x) x$name),
             loc=sapply(users,function(x) x$location),
             desc=sapply(users,function(x) x$description))

}