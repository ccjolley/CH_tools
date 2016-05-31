# Procedures for constructing social networks from CrimHex Facebook exports

library(stringr)
library(igraph)

# load data for testing
source('network_utils.R')
ws <- get_ws(dir="C:/Users/Craig/Desktop/Live projects/Libya/Python LDA",'USAID FB.*xls')

# Connect users who comment on the same post
# Bipartite network of posts and commenters
# Create projections for posts and users

# Do this using URLs: a typical comment will have a URL that looks like:
# https://www.facebook.com/62690599685/posts/10153174130824686?comment_id=10153174387569686
# The first number (62690599685) is an ID for the page
# The second number (10153174130824686) is an ID for the original post
# The third number  (10153174387569686) is an ID for the comment

bipartite_post_comment <- function(ws) {
  # Generates a bipartite network of posts and people who commented on that post.
  # The name of the posts is the post URL, the name of the users is their name
  # Vertices in the resulting graph are assigned a type of 'FALSE' if they are 
  # posts and 'TRUE' if they are users -- bipartite.projection() expects a 
  # logical value for vertex type.
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

# Connect users who post on the same public page
# Bipartite network of users and pages; create projections

# Connect users with similar topic preferences (requires LDA as input)
# Similarity metric: >1 overlap within the top n weighted topics
# Might be easiest to implement network construction in Python, export
# in some format, visualize in R (b/c it's prettier than matplotlib)

# Connect posts/pages with similar topic preferences (requires LDA as input)

# Connect words based on prominence in topics (requires LDA as input)
# Filter for words that have significant weight in at least one topic
# Connect words whose weight in the same topic exceeds a threshold
# Parameter tuning -- look for a percolation threshold where a giant
# connected component emerges