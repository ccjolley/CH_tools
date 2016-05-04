library(Rfacebook)

setwd("C:/Users/Craig/Dropbox/SMA")

# Testing out functions in Rfacebook package

# First, authorization -- doesn't work so well because I'd like those 
# extended permissions.
# my app is at https://developers.facebook.com/apps/613900445419140/settings/
id <- '613900445419140'
secret <- '66f8b90e2b64002a10eff111dcc8d7b5'
fb_oauth <- fbOAuth(app_id=id, app_secret=secret,extended_permissions = FALSE) 
save(fb_oauth, file="fb_oauth")
load("fb_oauth")

# Another approach: go to https://developers.facebook.com/tools/explorer/
token <- 'CAACEdEose0cBADZCSco3RoZCHqH8P3XPYSfWX4u0wlQDumtZBh1mORz0K0Wb8ySg5T4ZAZAAUI2w9B5g9JFcr886iIunOgdS2vxmSGXuX0Qjz2mJ6NVEx6g2smMIsnLmZCK2H8B4VtPGoEwgNnuc680AKGKmugZARczIbPLZCz4rveAfaugDADsV2uoupgrKsNls4tmUzsFLGSPWbhZABQ672'
# token only valid for 2h

me <- getUsers("me",token=token) 
my_friends <- getFriends(token, simplify=TRUE)
# only shows me friends who've used the 'app' -- in this case the FB Graph Explorer
# I only have one such friend
my_friends_info <- getUsers(my_friends$id, token=token, private_info=TRUE)

getLikes("me",token=token)
getLikes(my_friends$id[1],token=token) # Don has liked a lot more stuff than I have

# getNetwork() could be handy, if only I had more friends who'd used this app

lab <- getPage("GlobalDevLab",token=token,n=10)
# useful for getting likes, comments, shares -- does CH return this data?
# kind of -- can export admin post list, but it's not formatted very well for Excel
# also CH doesn't let me drill down and see which comments go with which post

post <- getPost(lab$id[1],token)
post$likes
# now I know who liked the post, but only a little more info about them
getUsers(post$likes$from_id[1],token=token)
getLikes(post$likes$from_id[1],token=token) # not authorized to find out what else they liked

# Bottom line here is that I have some ability to get information that isn't accessible
# in CH; particularly the likes and comments associated with specific posts on a public 
# page. The downside is that there's hardly any information I can collect on people, unless 
# they have used the app. Even if they use the 'app', you're still not accessing any
# interesting information about them, except their collection of likes.

# It's possible that the owner of a public page might be able to get more information
# about people who participate on that page -- I'll have to see.
