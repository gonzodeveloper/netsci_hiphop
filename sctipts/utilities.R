# Utility functions for the script

####################################################################
# Given a sequence of link community vertices in a reified graph
# Return the size of the largest link community


biggest_lc <- function(g, l_comms){
  sizes <- c()
  for(lc in l_comms){
    sizes <- c(sizes, length(adjacent_vertices(g, lc, mode = "all")[[1]]))
  }
  return(max(sizes))
}

######################################################################
# Below Functions provided by Dr. Dan Suthers


######################################################################
# Return top verices with top n values from graph

topnv <- function(graph, values, n=10) {
  return(V(graph)[sort.list(values, decreasing=TRUE)[1:n]])
}



######################################################################
# Get degrees, indegrees, or outdegrees without isolate nodes. 

nonzero_degrees <- function(g, mode="total") {
  d <- degree(g, mode=mode)
  return(d[d != 0])
}

######################################################################
# We often have to do the same sequence of operations to 
# initialize the distribution object.  This function
# modifies distribution object dis to have parameters set. 
# The given object is modified so there is no returned value. 

initialize_parameter_estimates <- function(dis) {
  dis$setXmin(estimate_xmin(dis))
  dis$setPars(estimate_pars(dis)) 
}

######################################################################
# Helpers that given the degrees (use one of the above helpers), 
# will return the indicated distribution (save it in a variable). 

initialize_disexp <- function(degrees) {
  dis <- disexp$new(degrees)
  initialize_parameter_estimates(dis)
  return(dis)
}

initialize_dislnorm <- function(degrees) {
  dis <- dislnorm$new(degrees)
  initialize_parameter_estimates(dis)
  return(dis)
}

initialize_displ <- function(degrees) {
  dis <- displ$new(degrees)
  initialize_parameter_estimates(dis)
  return(dis)
}

initialize_dispois <- function(degrees) {
  dis <- dispois$new(degrees)
  initialize_parameter_estimates(dis)
  return(dis)
}

######################################################################
# Utility Plotter 
# Uses poweRlaw package plotter for cumulative distribution functions. 
# Requires all four distributions. 
# lwd controls width of lines for distributions in legend
# lcex controls legend size 
# lpos controls legend position

plot_discrete_distributions <- function(title, disexp, dislnorm, displ, dispois,
                                        lwd=3, lcex=1.0, lpos="topright") {
  plot(disexp, main=title, xlab="k", ylab="p(k)")
  lines(dispois, col="orange", lwd=lwd) # Rarely matches: draw first 
  lines(disexp, col="green", lwd=lwd) 
  lines(dislnorm, col="red", lwd=lwd) 
  lines(displ, col="blue", lwd=lwd)     # Draw last so we can see it
  legend(lpos, 
         c(paste("exp, xmin =", disexp$getXmin()),
           paste("lnorm, xmin =", dislnorm$getXmin()),
           paste("pl, xmin =", displ$getXmin()),
           paste("pois, xmin =", dispois$getXmin())),
         col=c("green", "red", "blue", "orange"), 
         lty=1, lwd=lwd, cex=lcex)
}


######################################################################
# Utility Plotter 
# We will want to plot and compare to power law a lot, and compute
# metrics. This returns the displ object for the given graph in case
# you want to do compare_distributions and such. 
######################################################################
# g: an igraph 
# mode: "total", "in", "out"
# geodesic: mean_distance is slow. Set to FALSE to leave it out. 

plot_distribution_pl <- function(g, title, mode="all", geodesic=TRUE) {
  # Regular distribution with metrics 
  plot(degree_distribution(g, mode=mode), 
       main=paste(title, mode, "Log Log"), log="xy", xlab="k", ylab="p(k)")
  # c <- components(g) # See comment below 
  legend("topright", bty="n",
         c(paste("|V|=", vcount(g), " |E|=", ecount(g), sep=""),
           paste("Transitivity:  ", 
                 round(transitivity(g, type="global"), digits=4)),
           if (geodesic) {paste("Mean Geodesic: ", 
                                round(mean_distance(g), digits=2))},
           paste("Assortativity: ", 
                 round(assortativity_degree(g), digits=4))
           # This is always 1 with today's models. 
           # paste("Components: ", c$no)
         ))
  # CDF with power law fit. Draw others for reference 
  g.displ <- initialize_displ(nonzero_degrees(g, mode=mode))
  g.disexp <- initialize_disexp(nonzero_degrees(g, mode=mode))
  g.dislnorm <- initialize_dislnorm(nonzero_degrees(g, mode=mode))
  d <- degree(g, mode=mode)
  plot(g.displ, main=paste(title, mode, "CDF"), xlab="k", ylab="p(k)")
  # Plot the others first so blue is on top 
  lines(g.disexp, col="green", lwd=3)
  lines(g.dislnorm, col="red", lwd=3)
  lines(g.displ, col="blue", lwd=3)
  legend("bottomleft", bty="n", 
         c(paste("exp, xmin =", g.disexp$getXmin()),
           paste("lnorm, xmin =", g.dislnorm$getXmin()),
           paste("pl, xmin =", g.displ$getXmin())), 
         col=c("green", "red", "blue"), 
         lty=1, lwd=3)
  legend("topright", bty="n", 
         c(paste("k min: ", min(d)),
           paste("k max: ", max(d)), 
           paste("pl gamma: ", round(g.displ$pars, digits=2))))
  return(g.displ)
}




######################################################################
# Generic window creator for R 
# Dan Suthers, January 28, 2018 
# Linux option added Feburary 1, 2018 with help from Kyle Hart
# title: a string or null
# width, height: numeric or null 
# TODO: Windows options
######################################################################

new_window <- function(title=NULL, width=NULL, height=NULL){
  platform <- Sys.info()[[1]]
  if (platform =="Darwin") quartz(title, width, height)
  else if (platform == "Linux") x11(title=title, width=width, height=height)
  else dev.new()
  return(dev.cur())
}

######################################################################


######################################################################
# robustness_simulation 
# Iterates to delete one vertex each pass until only one remains, 
# recording the values of metrics for each pass. Returns a data frame
#   g: an igraph graph 
#   attack = TRUE: vertex is chosen randomly from those remaining 
#            vertices having the highest degree on each pass. 
#   attack = FALSE (default): vertex is chosen randomly from all 
#            remaining vertices each pass. 
#   cutoff = 1.0 (default): value in (0,1] indicating proportion
#            of the simulation to run, e.g., 0.05 means stop after 
#            5% of the vertices have been deleted. This enables
#            focusing on more plausible scenarios for real systems. 
#   meandist=FALSE (default): no distance metrics are computed 
#   meandist=TRUE: mean geodesic is computed each pass, which may be
#            slow for large graphs (see mean_distance documentation). 
# Value: A data frame with up to |V|-1 rows (depending on cutoff).
#   The first data row has metrics before any vertices are deleted, 
#   and the remaining rows have metrics after each vertex is deleted. 
#   If meandist=FALSE, columns include $numvert (number of vertices), 
#   $numcomp (number of components), and $bigcomp (percent vertices 
#   in largest component). If meandist=TRUE, an additional column 
#   $meandist (mean geodesic distance) is included. 
# Does not modify the argument graph. 
######################################################################

robustness_simulation <- function(g, attack=FALSE, cutoff=1.0, meandist=FALSE) {
  # tracks current number of vertices 
  numv <- vcount(g) 
  # How many vertices to delete. Since we record metrics before deleting
  # the number of data rows recorded will be deletions+1 
  deletions <- floor((numv - 1) * cutoff)
  
  # Create a data frame with one row per graph state and columns for
  # the metrics requested. Results are returned in this data frame. 
  if (meandist) {
    metrics <- data.frame(matrix(data = 0, nrow = deletions+1, ncol = 4))
    colnames(metrics) <- c("numvert", "numcomp", "bigcomp", "meandist")
  } else {
    metrics <- data.frame(matrix(data = 0, nrow = deletions+1, ncol = 3))
    colnames(metrics) <- c("numvert", "numcomp", "bigcomp")
  }
  
  # Record metrics for network before first deletion 
  metrics$numvert[1] <- numv
  comp <- components(g, mode="weak")
  metrics$numcomp[1] <- comp$no
  metrics$bigcomp[1] <- max(comp$csize)/numv
  if (meandist) {metrics$meandist[1] <- mean_distance(g)} # slow
  
  # Provide progress meter for larger runs 
  if (deletions > 99) {
    cat(paste("Deleting", deletions, "of", numv, "vertices (.=100): "))
  }
  
  # We used row 1 already and want to write to row i, starting at 2
  for (i in 2:(deletions+1)) { 
    
    # Progress meter
    if (deletions > 99) { 
      if (i %% 1000 == 0) {    
        cat("|")
      } else if (i %% 100 == 0) {
        cat(".")
      }
    }
    
    # Delete a vertex, randomly chosen unless this is an attack
    # in which case we randomly choose a highest degree vertex. 
    if (attack) {
      d <- degree(g)
      # Sample randomly from among those of max degree. 
      v <- sample(which(d==max(d)), 1)
      # for debugging
      # cat(paste("\n  deleting vertex", v, "of degree", max(d)))
    } else {
      v <- sample(numv, 1)
      # for debugging
      # cat(paste("\n  deleting vertex", v, "of degree", degree(g)[v]))
    }
    g <- delete_vertices(g, v)
    numv <- numv - 1 
    
    # Record updated metrics in row for current iteration. 
    metrics$numvert[i] <- numv
    comp <- components(g, mode="weak")
    metrics$numcomp[i] <- comp$no
    metrics$bigcomp[i] <- max(comp$csize)/numv
    if (meandist) {metrics$meandist[i] <- mean_distance(g)}
  }
  
  if (deletions > 99) {cat("\n")}
  return(metrics)
} 

######################################################################
# iterated_robustness_simulation
# Performs multiple trials of robustness_simulation, returning a data
# frame with the mean values for each trial. 
#   g, attack, cutoff, meandist: see robustness_simulation documentation 
#   trials=3 (default): number of trials of robustness_simulation
#   plottrials=FALSE(default): no plotting is done 
#   plottrials=TRUE: plot_robustness_results is called to plot the 
#     results of each trial, but not of the final result. This is 
#     useful for illustrating variation. (Attribute g$name is used 
#     to identify the graph in the plot.) 
# Value: Returns a data frame as described for robustness_simulation,
#   but values are the mean for each trial. 
# Does not modify the argument graph. 
######################################################################

iterated_robustness_simulation <- function(g, attack=FALSE, 
                                           cutoff=1.0, meandist=FALSE,
                                           trials=3, plottrials=FALSE) {
  
  # Tell the user what is about to happen; it could be slow.
  deletions <- floor((vcount(g) - 1) * cutoff)  # Just for their info
  name <- if (is.null(g$name)) {"Anonymous Graph"} else {g$name} 
  cat(paste0(
    if(attack) {"\nAttack on "} else {"\nRandom Failure of "}, 
    deletions,
    " vertices (", cutoff*100, "%) in ", name, ", ", trials, 
    if (meandist) {" Trials, with meandist\n"} else {" Trials\n"}))
  
  # First trial provides the data frame into which we accumlate results 
  cat("Trial 1: ")
  results <- robustness_simulation(g, attack=attack, cutoff=cutoff, 
                                   meandist=meandist)
  if (plottrials) {
    plot_robustness_results(results, meandist=meandist,         
                            name=paste(name, "Trial 1"))}
  
  # Remaining trials are accumulated into this data frame. 
  for (i in 2:trials) {
    cat(paste0("Trial ", i, ": "))
    metrics <- robustness_simulation(g, attack=attack, cutoff=cutoff, 
                                     meandist=meandist) 
    if (plottrials) {
      plot_robustness_results(metrics, meandist=meandist,         
                              name=paste(name, "Trial", i))}
    # I'm assuming these won't get large enough to overflow.
    results$numcomp <- results$numcomp + metrics$numcomp 
    results$bigcomp <- results$bigcomp + metrics$bigcomp
    if (meandist) {results$meandist <- results$meandist + metrics$meandist}
  }
  
  # Now take the average and return 
  results$numcomp <- results$numcomp / trials
  results$bigcomp <- results$bigcomp / trials
  if (meandist) {results$meandist <- results$meandist / trials}
  return(results)
}


######################################################################
# plot_robustness_results 
# Plots the results of a robustness simulation to the current device, 
# including number of components and fraction of vertices in largest 
# component, with optional plotting of geodesic distances.
#
#   metrics: data frame in format provided by robustness_simulation
#   name: string name used to identify the graph in the plots 
#   meandist=FALSE (default): geodesic means are not plotted (two plots)
#   meandist=TRUE: geodesic means are plotted (three plots)
#   yzero=FALSE (default): if TRUE, forces y axis to go to 0, which
#     may be useful for comparison across runs with different ranges, 
#     but for most simulations will leave a lot of whitespace. 
#
# Usage Note: No 'cutoff' parameter is available here because it is 
# not necessary. If your similation metrics include more iterations 
# than you want to plot, simply take a 'row slice', e.g., 
#   plot(metrics[1:100,] ...) 
# (the comma is needed) to plot metrics for the first 100 deletions. 
######################################################################

plot_robustness_results <- function(metrics, name = "", meandist = FALSE,
                                    yzero=FALSE) {
  
  plot(metrics$numvert, metrics$numcomp, 
       main = "Number of Components", 
       sub = name, cex.sub = 1.2, type = "l", lwd = "2", 
       xlab = "Vertices", ylab = "Components", 
       # Reverse the x axis to start with all vertices and see decline
       xlim = rev(range(metrics$numvert)),
       # if requested scale y axis to 0
       if (yzero) {ylim = c(0, max(range(metrics$numcomp)))}
  )
  
  plot(metrics$numvert, metrics$bigcomp, 
       main = "Proportion in Giant Component", 
       sub = name, cex.sub = 1.2, type = "l", lwd = "2", xlab = "Vertices", 
       ylab = "Proportion", xlim = rev(range(metrics$numvert)),
       if (yzero) {ylim = c(0, max(range(metrics$bigcomp)))}
  )
  
  if (meandist) {
    plot(metrics$numvert, metrics$meandist, 
         main = "Mean Distance", 
         sub = name, cex.sub = 1.2, type = "l", lwd = "2", 
         xlab = "Vertices", ylab = "Mean Distance", 
         xlim = rev(range(metrics$numvert)),
         if (yzero) {ylim = c(0, max(range(metrics$meandist)))}
    )
  }
}


# Increasing ego distance can be instructive about the relationship
# between local transitivity and betweenness 
make_and_plot_ego_graph <- function(g, e, order=1){
  eg <- make_ego_graph(g, order, e)[[1]]
  plot(eg, main=paste0(e$label, " Ego ", order, ", |V|=", vcount(eg)))
  return(eg)
}

######################################################################
# reify_link_communities 
# Adds vertices representing link communities to a graph, with the
# intention that they be used for visualization in Gephi. 
# Dan Suthers, Nov 16, 2016
# Comment added April 7, 2018 concerning computation of node.ids
######################################################################
library(igraph)
library(linkcomm)

# Community labels will be constructed from IDs to indicate 
# that they are communities. 
#
comm_label <- function (id) {return(paste0("COMM_", id))}

# Given a graph g and a legal link community object lc for
# that graph, returns a copy of the graph with communites
# added as vertices. We don't compute the link community
# within this function as we want the user to retain full
# control of that computation through its various parameters. 
# 
reify_link_communities <- function(g, lc) {
  
  #  Mark existing vertices as not being community nodes. 
  V(g)$comm_p <- FALSE 
  
  # Create a "community" vertex for each cluster. The cluster
  # column of attribute $nodeclusters has cluster ids. It's a
  # factor, so we can get the possible values with 'levels'. 
  for(id in levels(lc$nodeclusters$cluster)) {
    g <- add_vertices(g, 1, label = comm_label(id), comm_p = TRUE)
  }
  
  # Get list of regular vertices. 
  # Note that node.ids <- lc$nodeclusters$node did not work. 
  node.ids <- as.numeric(as.vector(lc$nodeclusters$node))
  
  # Make corresponding list of communities identified by label. 
  comm.labels <- vapply(lc$nodeclusters$cluster, comm_label, character(1))
  
  # Make the regular vertices point to their community vertices,
  # using 'which' to map from string labels to actual node IDs. 
  for (i in 1:length(node.ids)) {
    g <- g + edge(node.ids[i], which(V(g)$label == comm.labels[i]))
  }
  return(g)
}

######################################################################



# Pau