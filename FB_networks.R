# Procedures for constructing social networks from CrimHex Facebook exports

library(stringr)
library(igraph)
# library(Rfacebook)
# id <- '613900445419140'
# secret <- '66f8b90e2b64002a10eff111dcc8d7b5'
# fb_oauth <- fbOAuth(app_id=id, app_secret=secret,extended_permissions = FALSE) 
# save(fb_oauth, file="fb_oauth")
# load("fb_oauth")

# load data for testing
source('network_utils.R')
ws <- get_ws(dir="C:/Users/Craig/Desktop/Live projects/Libya/Python LDA",'USAID FB.*xls')

###############################################################################
# The key to building out networks is to use URLs. A typical comment will have 
# a URL that looks like:
# https://www.facebook.com/62690599685/posts/10153174130824686?comment_id=10153174387569686
# The first number (62690599685) is an ID for the page
# The second number (10153174130824686) is an ID for the original post
# The third number  (10153174387569686) is an ID for the comment
###############################################################################

###############################################################################
# Generates a bipartite network of posts and people who commented on that post.
# The name of the posts is the post URL, the name of the users is their name
# Vertices in the resulting graph are assigned a type of 'FALSE' if they are 
# posts and 'TRUE' if they are users -- bipartite.projection() expects a 
# logical value for vertex type.
###############################################################################
bipartite_post_comment <- function(ws) {
  wsComment <- ws[grep('comment_id=',ws$URL),]
  wsComment <- unique(wsComment)
  url_pattern <- '^(.*)?comment_id'
  wsComment$post_url <- sapply(wsComment$URL,function(x) 
    str_match(x,url_pattern)[1,2])
  edges <- data.frame(post=wsComment$post_url, 
                      commenter=wsComment$Author)
  g <- graph.data.frame(edges,directed=TRUE)
  V(g)[grep('http',V(g)$name)]$type <- FALSE
  V(g)[grep('http',V(g)$name,invert=TRUE)]$type <- TRUE
  g  
}

g <- bipartite_post_comment(ws) %>% graph_lcc()
sma_plot(g) # by default, the high-PR nodes will be commenters, not
            # original posts, because of edge directionality

bp <- bipartite.projection(g)
sma_plot(bp[[1]])

###############################################################################
# Generates a bipartite network of pages and people who posted on that page.
# Vertices in the resulting graph are assigned a type of 'FALSE' if they are 
# pages and 'TRUE' if they are users -- bipartite.projection() expects a 
# logical value for vertex type.
###############################################################################
bipartite_page_comment <- function(ws) {
  wsComment <- ws[grep('comment_id=',ws$URL),]
  wsComment <- unique(wsComment)
  wsAdmin <- ws[grep('comment_id=',ws$URL,invert=TRUE),]
  url_pattern <- '^(.*)\\?comment_id'
  main_pattern <- '^(.*)/posts'
  urlmatch <- data.frame(url=str_match(wsComment$URL,main_pattern)[,2] %>% unique())
  urlmatch$name <- sapply(urlmatch$url,function(s) {
    d <- wsAdmin[grep(s,wsAdmin$URL),'Author'] %>% table() %>% as.data.frame()
    d <- d[order(d$Freq,decreasing = TRUE),]
    as.character(d[1,1])
  })
  wsComment$page_name <- sapply(wsComment$URL, function(x) {
    url <- str_match(x,url_pattern)[1,2]
    res <- wsAdmin[wsAdmin$URL == url,'Author']
    main_url <- str_match(x,main_pattern)[1,2]
    if (length(unique(res)) == 1) { # original post contained in dataset
      return(res[1])
    } else { # use table instead
      return(urlmatch[urlmatch$url==main_url,'name'])
    }
  })
  edges <- data.frame(page=wsComment$page_name, 
                      commenter=wsComment$Author)
  g <- graph.data.frame(edges,directed=TRUE)
  V(g)$type <- TRUE
  V(g)[V(g)$name %in% urlmatch$name]$type <- FALSE
  g  
}


ws2 <- get_ws(dir="C:/Users/Craig/Desktop/SMA/Vietnam/","Vietnam enviro FB 0413.xls")  
g2 <- bipartite_page_comment(ws2) %>% graph_lcc()
sma_plot(g2)
sma_bar(g2) # again, giving us commenters, not pages (although some top commenters *are* pages)

# Connect users with similar topic preferences (requires LDA as input)
# Similarity metric: need to experiment with this
# Might be easiest to implement network construction in Python, export
# in some format, visualize in R (b/c it's prettier than matplotlib)

# Connect posts/pages with similar topic preferences (requires LDA as input)

# Connect words based on prominence in topics (requires LDA as input)
# Filter for words that have significant weight in at least one topic
# Connect words whose weight in the same topic exceeds a threshold
# Parameter tuning -- look for a percolation threshold where a giant
# connected component emerges