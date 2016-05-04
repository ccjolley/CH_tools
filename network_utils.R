# Import tweet data from Crimson Hexagon and analyze using igraph
library(igraph)
library(XLConnect)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(dplyr)

###############################################################################
# Load CH output data
###############################################################################
get_ws <- function(dir,patterns) {
  setwd(dir)
  fnames <- unlist(sapply(patterns, function(x) list.files(pattern=x)))
  print(fnames)
  ldply(fnames, function(x) readWorksheetFromFile(x,sheet=1))
}

###############################################################################
ws_to_graph <- function(ws){
  wsRT <- ws[grep("^RT ",ws$Contents),] # keep only retweets
  wsRT <- unique(wsRT)  
  edges <- data.frame(rt=wsRT$Author, 
                      orig=sapply(strsplit(wsRT$Contents,' '),
                                  function(x) gsub(':$','',x[2],perl=TRUE)))
   graph.data.frame(edges,directed=TRUE)
}
###############################################################################
graph_lcc <- function(g) {
  connected <- clusters(g,mode='weak')
  lcc_v <- V(g)[connected$membership==which.max(connected$csize)]
  induced.subgraph(g,lcc_v)  
}


###############################################################################
# Plot a network map of a social network.
# g = igraph object
# n = number of nodes to highlight
# highlight = results of a scoring function (e.g. page.rank) according to which
#             highlighted nodes should be chosen
# layout = results of an igraph layout routine
# extra = additional nodes to highlight
###############################################################################
sma_plot <- function(g,n=8,highlight=NULL,layout=NULL,extra=NULL) {
  if (is.null(layout)) {
    layout <- layout.fruchterman.reingold(g)
  }
  if (is.null(highlight)) {  # use PageRank by default
    highlight <- page.rank(g)
  }
  if (length(extra) > n) {
    n <- length(extra)
  }
  col_n <- brewer.pal(n,'Dark2')
  if ("vector" %in% names(highlight)) {
    top <- highlight$vector %>% sort(.,decreasing=TRUE) %>% head(.,n-length(extra)) %>% 
      c(.,highlight$vector[extra]) %>% names() %>% match(.,names(highlight$vector))
  } else {
    top <- highlight %>% sort(.,decreasing=TRUE) %>% head(.,n-length(extra)) %>% 
      c(.,highlight[extra]) %>% names() %>% match(.,names(highlight))
  }
  E(g)$color <- "gray"  
  V(g)$shape <- "none"
  V(g)$size <- 0
  V(g)$color <- "gray"
  for(i in 1:n) {
    x <- top[i]
    E(g)[incident(g,x,mode="all")]$color <- col_n[i]
    V(g)[x]$shape <- "circle"
    V(g)[x]$size <- 4
    V(g)[x]$color <- col_n[i]
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

###############################################################################
# Horizontal bar plot to accompany network map
###############################################################################
sma_bar <- function(g,n=8,highlight=NULL,extra=NULL) {
  if (is.null(highlight)) {  # use PageRank by default
    highlight <- page.rank(g)
  }
  if (length(extra) > n) {
    n <- length(extra)
  }
  if ("vector" %in% names(highlight)) {
    prtop <- highlight$vector %>% sort(.,decreasing=TRUE) %>% 
      head(.,n-length(extra)) %>% c(.,highlight$vector[extra])
  } else {
    prtop <- highlight %>% sort(.,decreasing=TRUE) %>% 
      head(.,n-length(extra)) %>% c(.,highlight[extra])
  }
  prdf <- data.frame(name=names(prtop),pr=prtop)
  # re-order
  prdf$name <- factor(prdf$name,levels=rev(as.character(prdf$name)))
  col_n <- brewer.pal(n,'Dark2')
  prdf$color <- col_n
  ggplot(prdf,aes(x=name,y=pr)) +
    geom_bar(stat='identity',fill=rev(col_n),alpha=0.4) +
    coord_flip() +
    theme_classic() +
    theme(text=element_text(size=20),
          axis.title.x=element_blank(),
          axis.line.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.line.y=element_blank(),
          axis.ticks.y=element_blank())
}

