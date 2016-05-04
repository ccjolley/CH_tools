###############################################################################
# Wrapper function
###############################################################################
get_metric <- function(g,metric,n) {
  m <- metric(g)
  if ("vector" %in% names(m)) {
    return(m$vector[n])
  } else {
    return(m[n])
  }
}

###############################################################################
# Calculate metric for an alternate graph in which a new edge exists between
# n1 (retweeter) and n2 (source)
###############################################################################
try_edge <- function(g,n1,n2,metric) {
  new_g <- g + edge(n1,n2)
  get_metric(new_g,metric,n1)
}

###############################################################################
# Find the new edge originating at n1 (retweeter) that does the most to 
# improve metric. If this search is prohibitively slow, offer a list of
# suggestions identified by some other means.
#
# I'm assuming here that we want to increase the value of the metric; might
# need to change that in the future.
###############################################################################
suggest_edge <- function(g,n1,metric,search_list=NULL,verbose=TRUE) {
  if (is.null(search_list)) {
    search_list <- V(g)$name 
  }
  # set starting point
  m <- metric(g)
  best_val <- get_metric(g,metric,n1)
  best_n2 <- ''
  if (verbose==TRUE) {
    print(paste(best_n2,best_val))
  }
  for (n2 in search_list) {
    new_val <- try_edge(g,n1,n2,metric)
    if (new_val > best_val) {
      best_n2 <- n2
      best_val <- new_val
      if (verbose==TRUE) {
        print(paste(best_n2,best_val))
      }
    }
  }
  return(best_n2)
}

##############################################################################
# Suggest a series of edges that will maximize metric at each step. This 
# should get us a more diverse set of suggested interaction partners than just
# a single step.
#
# If unique = TRUE, don't suggest the same node more than once.
###############################################################################
suggest_edge_series <- function(g,n1,metric,num_steps,search_list=NULL,
                                verbose=TRUE,uniq_n2=TRUE) {
  if (is.null(search_list)) {
    search_list <- V(g)$name 
  }
  new_g <- g
  if (verbose) {
    print(paste('initial',get_metric(g,metric,n1),sep=': '))
  }
  for (i in 1:num_steps) {
    n2_i <- suggest_edge(new_g,n1,metric,search_list,verbose=FALSE)
    new_g <- new_g + edge(n1,n2_i)
    if (uniq_n2) {
      search_list <- search_list[search_list != n2_i]
    }
    if (verbose) {
      print(paste(n2_i,get_metric(new_g,metric,n1),sep=': '))
    }
  }
  new_g
}

