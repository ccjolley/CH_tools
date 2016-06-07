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
mayors <- function(g,comm,n=8,metric=page.rank,extra=NULL) {
  # for a community breakdown c of a graph g, return the mayors of the top n
  # communities
  if (max(membership(comm)) < n) {
    n <- max(membership(comm))
  }
  t_all <- membership(comm) %>% table() %>% as.data.frame()
  t_all <- t_all[order(t_all$Freq,decreasing=TRUE),] 
  t <- head(t_all,n)
  for (i in 1:length(extra)) {
    t <- rbind(t,t_all[t_all$.==extra[i],])
  }
  res <- sapply(1:nrow(t),function(i) {
    cnum <- t[i,1]
    vi <- V(g)[membership(comm)==cnum]
    gi <- induced.subgraph(g,vi)
    scores <- metric(gi)
    if ('vector' %in% names(scores)) { # account for different metrics
      scores <- scores$vector
    }
    names(scores)[which.max(scores)]
  })
  names(res) <- as.character(t[,1])
  res
}

# mayors(g,fg) 

###############################################################################
community_plot <- function(g,c,n=8,layout=NULL,showMayors=TRUE,extra=NULL) {
  # plot a graph g using a community breakdown c
  # color in the first n communities
  if (is.null(layout)) {
    layout <- layout.fruchterman.reingold(g)
  }
  if (max(membership(c)) < n) {
    n <- max(membership(c))
  }
  t_all <- membership(c) %>% table() %>% as.data.frame()
  t_all <- t_all[order(t_all$Freq,decreasing=TRUE),] 
  t <- head(t_all,n)
  for (i in 1:length(extra)) {
    t <- rbind(t,t_all[t_all$.==extra[i],])
  }
  col_n <- brewer.pal(nrow(t),'Dark2')
  V(g)$shape <- "none"
  V(g)$size <- 0
  V(g)$color <- "gray"
  source_nodes <- tail_of(g,E(g))
  target_nodes <- head_of(g,E(g))
  source_m <- membership(c)[source_nodes]
  target_m <- membership(c)[target_nodes]
  both_m <- rep(-1,length(E(g)))
  both_m[source_m==target_m] <- source_m[source_m==target_m]
  edge_colors <- rep("gray",length(E(g)))
  for (i in 1:nrow(t)) {
    edge_colors[both_m==t[i,1]] <- col_n[i]
  }
  E(g)$color <- edge_colors
  if (showMayors) {
    m <- mayors(g,c,n=n,extra=extra)
    for (i in 1:length(m)) {
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
  # document-term matrix in which urls, excess whitespace, most punctuation, and 
  # English stopwords have been removed.
  # Keeping '@' and '#' characters because of their special Twitter meaning.
  # Also convert to lowercase and keep only terms appearing in 0.01% or more
  # of documents.
  corpus <- VCorpus(VectorSource(ws$Contents))
  no_url <- content_transformer(function(x) gsub('https?:\\/\\/[a-zA-Z0-9.\\/]*','',x))
  corpus <- tm_map(corpus,no_url)
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus,content_transformer(tolower))
  # This doesn't work quite right yet
  mostPunct <- content_transformer(function(x) 
    gsub('[!"$%&\'()*+\\,\\.\\/:;<=>?\\^_`\\{\\|\\}~]',' ',x))
  corpus <- tm_map(corpus,mostPunct)
  #corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  dtm <- DocumentTermMatrix(corpus)
  removeSparseTerms(dtm,0.999) 
}
###############################################################################
cluster_dtm <- function(dtm,c,ws) {
  # use this to save time if you'll be runninng community_topics() repeatedly
  m <- as.matrix(dtm)
  clust_assign <- membership(c)[ws$Author]
  clust_assign[is.na(clust_assign)] <- -1
  cluster.list <- by(m, clust_assign, colSums)
  cluster.dtm <- matrix(unlist(cluster.list), nrow = length(cluster.list), byrow = T)
  colnames(cluster.dtm) <- names(cluster.list[[1]])
  rownames(cluster.dtm) <- names(cluster.list)
  cluster.dtm
}


###############################################################################
community_topics <- function(c,ws,dtm=NULL,n=8,num_keywords=10,extra=NULL,
                             cdtm=NULL) {
  # given a document-term matrix and a community breakdown, aggregate documents
  # to return a new dtm with only one document per community. 
  # For each of the top n communities in a clustered dtm, output the terms
  # with the highest TF-IDF weight
  if (is.null(dtm) & is.null(cdtm)) {
    print('ERROR in community_topics(): Must specify either dtm or cdtm!')
    return(NULL)
  }
  if (is.null(cdtm)) {
    cdtm <- cluster_dtm(dtm,c,ws)
  }
  t_all <- membership(c) %>% table() %>% as.data.frame()
  t_all <- t_all[order(t_all$Freq,decreasing=TRUE),] 
  t <- head(t_all,n)
  for (i in 1:length(extra)) {
    t <- rbind(t,t_all[t_all$.==extra[i],])
  }
  dtm_w <- as.DocumentTermMatrix(cdtm,weighting=weightTf) %>% weightTfIdf()
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


