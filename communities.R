setwd("C:/Users/Craig/Desktop/SMA/CH_tools")
source('network_utils.R')

library(tm)

# Benchmarking of community detection on a small and a large network

# g <- get_ws(dir="C:/Users/Craig/Desktop/SMA/Exchange",'Uganda-1215.xls') %>%
#   ws_to_graph() %>%
#   graph_lcc() %>%
#   as.undirected() %>%
#   simplify()
# 
# system.time(lp <- label.propagation.community(g))   # 0s
# system.time(fg <- fastgreedy.community(g))          # 0.01s
# system.time(le <- leading.eigenvector.community(g)) # 0.56s
# system.time(wt <- walktrap.community(g))            # 0.04s
# system.time(im <- infomap.community(g))             # 0.44s
# system.time(eb <- edge.betweenness.community(g))    # 18.62s
# system.time(sg <- spinglass.community(g))           # 27.52s

# g_big <- get_ws(dir="C:/Users/Craig/Desktop/Live projects/USAID Digital",'DigIncl 0531.*xls') %>%
#   ws_to_graph() %>%
#   graph_lcc() %>%
#   as.undirected() %>%
#   simplify()
# 
# system.time(lp <- label.propagation.community(g_big))   # 0.42s
# system.time(fg <- fastgreedy.community(g_big))          # 2.42s
# system.time(le <- leading.eigenvector.community(g_big)) # 3.78s
# system.time(wt <- walktrap.community(g_big))            # 17.89s
# system.time(im <- infomap.community(g_big))             # 34.23s
# system.time(eb <- edge.betweenness.community(g_big))    # didn't run
# system.time(sg <- spinglass.community(g_big))           # didn't run

###############################################################################
matching_edge <- function(g1,e,g2) {
  # given an edge e in g1, return the corresponding edge in g2
  name1 <- V(g1)[get.edges(g1,e)[1,1]]$name
  name2 <- V(g1)[get.edges(g1,e)[1,2]]$name
  E(g2)[get.edge.ids(g2,c(name1,name2))]
}


###############################################################################
mayors <- function(g,c,n=8,metric=page.rank) {
  # for a community breakdown c of a graph g, return the mayors of the top n
  # communities
  if (max(membership(c)) < n) {
    n <- max(membership(c))
  }
  sapply(1:n,function(i) {
    vi <- V(g)[membership(c)==i]
    gi <- induced.subgraph(g,vi)
    scores <- metric(gi)
    if ('vector' %in% names(scores)) { # account for different metrics
      scores <- scores$vector
    }
    names(scores)[which.max(scores)]
  })
}

# mayors(g,fg) 

###############################################################################
community_plot <- function(g,c,n=8,layout=NULL,showMayors=TRUE) {
  # plot a graph g using a community breakdown c
  # color in the first n communities
  if (is.null(layout)) {
    layout <- layout.fruchterman.reingold(g)
  }
  if (max(membership(c)) < n) {
    n <- max(membership(c))
  }
  col_n <- brewer.pal(n,'Dark2')
  E(g)$color <- "gray"  
  V(g)$shape <- "none"
  V(g)$size <- 0
  V(g)$color <- "gray"
  for (i in 1:n) {
    vi <- V(g)[membership(c)==i]
    gi <- induced.subgraph(g,vi)
    for (e in E(gi)) {
      eg <- matching_edge(gi,e,g)
      E(g)[eg]$color <- col_n[i]
    }
  }
  if (showMayors) {
    m <- mayors(g,c)
    for (i in 1:n) {
      x <- m[i]
      V(g)[x]$shape <- "circle"
      V(g)[x]$size <- 4
      V(g)[x]$color <- col_n[i]
    }
  }
  plot(g,
       layout=layout,
       vertex.label=NA,
       vertex.shape="none",
       vertex.size=0,
       edge.arrow.mode=0,
       edge.width=1) 
  # plot again, hiding the gray edges so that the colored ones are visible
  E(g)[E(g)$color=="gray"]$color  <- NA
  plot(g,
       layout=layout,
       add=TRUE,
       vertex.label=NA,
       edge.arrow.mode=0,
       edge.width=1) 
}

# g_layout <- layout.fruchterman.reingold(g)
# community_plot(g,fg,layout=g_layout)

###############################################################################
ws_to_dtm <- function(ws) {
  # Beginning with an Excel worksheet from Crimson Hexagon, generate a 
  # document-term matrix in which urls, excess whitespace, punctuation, and 
  # English stopwords have been removed.
  # Also convert to lowercase and keep only terms appearing in 0.01% or more
  # of documents.
  corpus <- VCorpus(VectorSource(ws$Contents))
  no_url <- content_transformer(function(x) gsub('https?:\\/\\/[a-zA-Z0-9.\\/]*','',x))
  corpus <- tm_map(corpus,no_url)
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus,content_transformer(tolower))
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  dtm <- DocumentTermMatrix(corpus)
  removeSparseTerms(dtm,0.999) 
}

###############################################################################
cluster_dtm <- function(dtm,c) {
  # given a document-term matrix and a community breakdown, aggregate documents
  # to return a new dtm with only one document per community. 
  m <- as.matrix(dtm)
  cluster.list <- by(m, clust_assign, colSums)
  cluster.dtm <- matrix(unlist(cluster.list), nrow = length(cluster.list), byrow = T)
  colnames(cluster.dtm) <- names(cluster.list[[1]])
  rownames(cluster.dtm) <- names(cluster.list)
  as.DocumentTermMatrix(cluster.dtm,weighting=weightTf)
}

###############################################################################
community_topics <- function(dtm,c,n=8,num_keywords=10) {
  # For each of the top n communities in a clustered dtm, output the terms
  # with the highest TF-IDF weight
  dtm_w <- weightTfIdf(dtm)
  t <- membership(c) %>% table() %>% as.data.frame()
  t <- t[order(t$Freq,decreasing=TRUE),] %>% head(.,n)
  res <- sapply(t$.,function(x) {
    row <- data.frame(name=colnames(dtm_w),
                      val=t(as.matrix(dtm_w[as.character(x),])))
    row$name <- as.character(row$name)
    row <- row[order(row[,2],decreasing=TRUE),]
    head(row$name,10)
  })
  colnames(res) <- as.character(t$.)
  res
}

###############################################################################
# dtm <- get_ws(dir="C:/Users/Craig/Desktop/SMA/Exchange",'Uganda-1215.xls') %>%
#   ws_to_dtm() %>%
#   cluster_dtm(.,fg)
# 
# community_topics(dtm,fg)


