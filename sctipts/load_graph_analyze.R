#############################################################################
# Kyle Hart
# University of Hawaii, MS Candidate
# Spring 2018
#
# Project: Comparison communities of artists in two different
# genres: hip-hop and techno
#############################################################################

#############################################################################
# LOADING THE GRAPHS AND CREATING BIPARTITE PROJECTIONS 
#############################################################################
library(igraph)
library(poweRlaw)
library(linkcomm)
setwd("~/repo/netsci_hiphop/")
source('sctipts/utilities.R')

# Load hip-hop artist-release edge list, make graph 
df <- read.csv("data/hip_hop_groups_release.csv")
HH <- graph.data.frame(df)

# Get a projection off the bipartite graph
V(HH)$type <- bipartite_mapping(HH)$type
HH.proj <- bipartite_projection(HH)
HH.artists <- HH.proj$proj1

# Add label attribute, remove name (otherwise reify link comm will fail)
V(HH.artists)$label <-V(HH.artists)$name
HH.artists <- delete_vertex_attr(HH.artists, "name")

# Summarize and save
summary(HH.artists)
write_graph(HH.artists, "graphs/hip_hop_artists.graphml",
            format = "graphml")

# Load techno artist-release edge list, make graph 
df <- read.csv("data/techno_artist_release.csv")
TC <- graph.data.frame(df)

# Same operation as above
V(TC)$type <- bipartite_mapping(TC)$type
TC.proj <- bipartite_projection(TC)
TC.artists <- TC.proj$proj1

summary(TC.artists)
write_graph(HH.artists, "graphs/techno_artists.graphml",
            format = "graphml")

# After initial inspection in Gehpi we can see that there is a release with
# way too many colaborating artists messing up the graph
V(TC)[which.max(degree(TC))]
max(degree(TC))               
# No way 249 artists colaborated on a single album
TC <- delete_vertices(TC, V(TC)[which.max(degree(TC))])

# Redo bipartite projection
V(TC)$type <- bipartite_mapping(TC)$type
TC.proj <- bipartite_projection(TC)
TC.artists <- TC.proj$proj1

# Add label attribute, remove name (otherwise reify link comm will fail)
V(TC.artists)$label <-V(TC.artists)$name
TC.artists <- delete_vertex_attr(TC.artists, "name")

# Summary and Save (notice that this single removal subtracted 22,986 edges from the projection)
summary(TC.artists)
write_graph(HH.artists, "graphs/techno_artists_fixed.graphml",
            format = "graphml")

#############################################################################
# RUNNING SIMPLE METRICS TO COMPARE EACH GRAPH
#############################################################################

# Number of verticies N
(HH.N <- vcount(HH.artists))        # 10117
(TC.N <- vcount(TC.artists))        # 6078

# Number of Edges
ecount(HH.artists)                  # 13845
ecount(TC.artists)                  # 5688
# Number of Components
length(sort(components(HH.artists)$csize))  # 2499
length(sort(components(TC.artists)$csize))  # 1859

# Giant Componnet Hip Hop
HH.components <- components(HH.artists)
big <- which(HH.components$csize == max(HH.components$csize))
HH.giant.nodes <- V(HH.artists)[which(HH.components$membership == big)]
HH.giant <- induced_subgraph(HH.artists, HH.giant.nodes) 
summary(HH.giant)          # |V| =  3856    |E| = 9301

# Giant Component Techno
TC.components <- components(TC.artists)
big <- which(TC.components$csize == max(TC.components$csize))
TC.giant.nodes <- V(TC.artists)[which(TC.components$membership == big)]
TC.giant <- induced_subgraph(TC.artists, TC.giant.nodes) 
summary(TC.giant)          # |V| = 1538     |E| = 2169


# Average Degree <k>
(HH.k <- mean(degree(HH.artists)))     # 2.736977
(TC.k <- mean(degree(TC.artists)))     # 1.733465


# Calculating Regimes
log(HH.N)             # With a random graph modeling these networks we would
log(TC.N)             # expect both graphs to be in supercritical regime


# Geodesic Distances
mean_distance(HH.artists)              # 5.6226
mean_distance(TC.artists)              # 9.3225

# Small World?
log(HH.N)/log(HH.k) 
log(TC.N)/log(TC.k)

# Diameter
diameter(HH.artists)  # 26
diameter(TC.artists)  # 33

# Global Transitivity
transitivity(HH.artists, type = "global")    # 0.23438  
transitivity(TC.artists, type = "global")    # 0.3475541 

# Average Local Transitivity
transitivity(HH.artists, type = "localaverage")    # 0.6365912  
transitivity(TC.artists, type = "localaverage")    # 0.5244994 

# Degree Assortativity
assortativity_degree(HH.artists)       # 0.2149805
assortativity_degree(TC.artists)       # 0.3401893


#############################################################################
# CENTRALITIES AND TOP NODES
#############################################################################

# Add cetrality atributes to each graph
V(HH.artists)$degree <- degree(HH.artists)
V(HH.artists)$strength <-strength(HH.artists)
V(HH.artists)$eigen_centrality <- eigen_centrality(HH.artists)$vector
V(HH.artists)$page_rank <- page_rank(HH.artists)$vector
V(HH.artists)$betweenness <- betweenness(HH.artists, normalized=TRUE)
V(HH.artists)$closeness <- closeness(HH.artists, normalized=TRUE)

V(TC.artists)$degree <- degree(TC.artists)
V(TC.artists)$strength <-strength(TC.artists)
V(TC.artists)$eigen_centrality <- eigen_centrality(TC.artists)$vector
V(TC.artists)$page_rank <- page_rank(TC.artists)$vector
V(TC.artists)$betweenness <- betweenness(TC.artists, normalized=TRUE)
V(TC.artists)$closeness <- closeness(TC.artists, normalized=TRUE)

# Print results for each
V(HH.artists)[order(V(HH.artists)$degree, decreasing=TRUE)[1:10]]$label
#[1] "Snoop Dogg"        "Lil Wayne"         "Jay-Z"            
#[4] "Busta Rhymes"      "Pharrell Williams" "Kanye West"       
#[7] "Nas"               "50 Cent"           "Akon"             
#[10] "Lil' Jon" 
V(HH.artists)[order(V(HH.artists)$strength, decreasing=TRUE)[1:10]]$label
#[1] "Snoop Dogg"       "Lil Wayne"        "50 Cent"         
#[4] "Method Man"       "RZA"              "Ghostface Killah"
#[7] "Jay-Z"            "GZA"              "Lil' Jon"        
#[10] "Raekwon" 
topnv(HH.artists, V(HH.artists)$eigen_centrality)$label
#[1] "Ghostface Killah"  "Raekwon"           "Method Man"       
#[4] "RZA"               "Inspectah Deck"    "GZA"              
#[7] "Ol' Dirty Bastard" "Wu-Tang Clan"      "Masta Killa"      
#[10] "U-God" 
topnv(HH.artists, V(HH.artists)$page_rank)$label
#[1] "Snoop Dogg"        "Lil Wayne"         "Jay-Z"            
#[4] "Lil' Jon"          "50 Cent"           "Pharrell Williams"
#[7] "Busta Rhymes"      "Kanye West"        "Method Man"       
#[10] "Nas"  
topnv(HH.artists, V(HH.artists)$betweenness)$label
#[1] "Snoop Dogg"        "Jay-Z"             "Nas"              
#[4] "Busta Rhymes"      "KRS-One"           "Lil Wayne"        
#[7] "Pharrell Williams" "Kanye West"        "Eminem"           
#[10] "Mos Def"
topnv(HH.artists, V(HH.artists)$closeness)$label
#[1] "Snoop Dogg"        "Pharrell Williams" "Busta Rhymes"     
#[4] "Nas"               "T.I."              "50 Cent"          
#[7] "Lil Wayne"         "Kanye West"        "Ludacris"         
#[10] "Mary J. Blige"  



V(TC.artists)[order(V(TC.artists)$degree, decreasing=TRUE)[1:10]]$label
#[1] "Adam Beyer"           "D.A.V.E. The Drummer"
#[3] "Cari Lekebusch"       "Mark Broom"          
#[5] "Jack Wax"             "Ant"                 
#[7] "Sterling Moss"        "Ben Sims"            
#[9] "Guy McAffer"          "Chris Liberator" 
V(TC.artists)[order(V(TC.artists)$strength, decreasing=TRUE)[1:10]]$label
#[1] "Adam Beyer"           "Mark Broom"          
#[3] "D.A.V.E. The Drummer" "Sterling Moss"       
#[5] "Jack Wax"             "Ant"                 
#[7] "Ben Sims"             "Cari Lekebusch"      
#[9] "Benji303"             "Chris Liberator"  
topnv(TC.artists, V(TC.artists)$eigen_centrality)$label
#[1] "Adam Beyer"     "Cari Lekebusch" "Patrice Scott" 
#[4] "Muslimgauze"    "C_C"            "Genghis (2)"   
#[7] "Andy Vaz"       "Metek"          "Underspreche"  
#[10] "powwowW" 
topnv(TC.artists, V(TC.artists)$page_rank)$label
#[1] "Mark Broom"           "D.A.V.E. The Drummer"
#[3] "Adam Beyer"           "Ben Sims"            
#[5] "Ant"                  "Guy McAffer"         
#[7] "Sterling Moss"        "Alex Calver"         
#[9] "Gary Beck"            "Jack Wax" 
topnv(TC.artists, V(TC.artists)$betweenness)$label
#[1] "Mark Broom"           "D.A.V.E. The Drummer"
#[3] "Pounding Grooves"     "Ben Sims"            
#[5] "Ant"                  "DJ Ogi"              
#[7] "Green Velvet"         "Adam Beyer"          
#[9] "Mr. G"                "Bad Boy Pete"  
topnv(TC.artists, V(TC.artists)$closeness)$label
#[1] "Mark Broom"           "Ben Sims"            
#[3] "Pounding Grooves"     "D.A.V.E. The Drummer"
#[5] "Mr. G"                "Markus Suckut"       
#[7] "Chris Liebing"        "Edit Select"         
#[9] "Davide Squillace"     "Terry Brookes" 

# We will refer to these 5 as the Artists of Interest (AoI) for reasons explained later
# they will be used again in the overlapping community section


# Ego Networks for Hip Hop Only
(snoop <- V(HH.artists)[which(V(HH.artists)$label=="Snoop Dogg")])
(nas <- V(HH.artists)[which(V(HH.artists)$label=="Nas")])
(lil_jon <- V(HH.artists)[which(V(HH.artists)$label=="Lil' Jon")])
(ghostface <- V(HH.artists)[which(V(HH.artists)$label=="Ghostface Killah")])
(pharell <- V(HH.artists)[which(V(HH.artists)$label=="Pharrell Williams")])

AoI <- c(snoop, nas, lil_jon, ghostface, pharell)

HH.snoop <- make_and_plot_ego_graph(HH.artists, snoop, 1)
HH.nas <- make_and_plot_ego_graph(HH.artists, nas, 1)
HH.lil_jon <- make_and_plot_ego_graph(HH.artists, lil_jon, 1)
HH.ghostface <- make_and_plot_ego_graph(HH.artists, ghostface, 1)
HH.pharell <- make_and_plot_ego_graph(HH.artists, pharell, 1)


# Average Transitvities
transitivity(HH.artists, type = "localaverage") # 0.6365912
transitivity(HH.snoop, type = "localaverage") # 0.7571265
transitivity(HH.nas, type = "localaverage") # 0.8160505
transitivity(HH.lil_jon, type = "localaverage") # 0.7595058
transitivity(HH.ghostface, type = "localaverage") # 0.8791565
transitivity(HH.pharell, type = "localaverage") # 0.7987071

assortativity.degree(HH.artists, directed = FALSE) # [1] 0.2149805
assortativity.degree(HH.snoop, directed = FALSE)   # [1] -0.2429614
assortativity.degree(HH.nas, directed = FALSE)     # [1] -0.2994587
assortativity.degree(HH.lil_jon, directed = FALSE) # [1] -0.3484328
assortativity.degree(HH.ghostface, directed = FALSE) # [1] -0.2488484
assortativity.degree(HH.pharell, directed = FALSE) # [1] -0.2524449



HH.snoop.mc <- max_cliques(HH.snoop)
table(sapply(HH.snoop.mc, length))
# 2  3  4  5  6  7 13 
#16 55 48 14  4  2  1 

HH.nas.mc <- max_cliques(HH.nas)
table(sapply(HH.nas.mc, length))
# 2  3  4  5  6 12 
#11 23 14  1  2  1 

HH.lil_jon.mc <- max_cliques(HH.lil_jon)
table(sapply(HH.lil_jon.mc, length))
# 2  3  4  5  6  7  8 
#3 25  6  3  6  7  2 

HH.ghostface.mc <- max_cliques(HH.ghostface)
table(sapply(HH.ghostface.mc, length))
# 2  3  4  7 11 12 
#5  3  2  1  6  4 

HH.pharell.mc <- max_cliques(HH.pharell)
table(sapply(HH.pharell.mc, length))
# 2  3  4  5  6  7 13 
#3 20 27 10  2  1  1 

write_graph(HH.snoop, "graphs/snoop.graphml", format = "graphml")
write_graph(HH.nas, "graphs/nas.graphml", format = "graphml")
write_graph(HH.lil_jon, "graphs/lil_jon.graphml", format = "graphml")
write_graph(HH.ghostface, "graphs/ghostface.graphml", format = "graphml")
write_graph(HH.pharell, "graphs/pharell.graphml", format = "graphml")





#############################################################################
# PLOTTING
#############################################################################

# Plotting degree distributions
plot(degree_distribution(HH.artists), 
     main = "Hip Hop Artists Degree Distribution (log-log)", xlab = "k", ylab = "p(k)", log="xy")
plot(degree_distribution(TC.artists), 
     main = "Techno Artists Degree Distribution (log-log)", xlab = "k", ylab = "p(k)", log="xy")

plot(degree_distribution(HH.artists, cumulative = TRUE),
     main = "Cumulative Hip Hop Artists Degree Distribution (log-log)",  xlab = "k", ylab = "p(k)", log="xy")
plot(degree_distribution(TC.artists, cumulative = TRUE), 
     main = "Cumulative Techno Artists Degree Distribution (log-log)",  xlab = "k", ylab = "p(k)", log="xy")


HH.artists.degs <- nonzero_degrees(HH.artists)
TC.artists.degs <- nonzero_degrees(TC.artists)

HH.disexp <- initialize_disexp(HH.artists.degs)
HH.dislnorm <- initialize_dislnorm(HH.artists.degs)
HH.displ <- initialize_displ(HH.artists.degs)
HH.dispois <- initialize_dispois(HH.artists.degs)

TC.disexp <- initialize_disexp(TC.artists.degs)
TC.dislnorm <- initialize_dislnorm(TC.artists.degs)
TC.displ <- initialize_displ(TC.artists.degs)
TC.dispois <- initialize_dispois(TC.artists.degs)



# We can see that neither of the networks' degree distributions are particularly well 
# modeled by Poisson, Exponential, Log Normal or Power Law Models
new_window()
plot_discrete_distributions("Hip Hop Distribution Comparison", HH.disexp, HH.dislnorm, HH.displ, HH.dispois)
new_window()
plot_discrete_distributions("Teechno Distribution Comparison", TC.disexp, TC.dislnorm, TC.displ, TC.dispois)


(HH.dislnorm$setXmin(HH.displ$getXmin()))
HH.dislnorm$setPars(estimate_pars(HH.dislnorm)) 
HH.pl.vs.lnorm <- compare_distributions(HH.dislnorm, HH.displ)
HH.pl.vs.lnorm$p_two_sided          # 0.000498521
HH.pl.vs.lnorm$p_one_sided          # 0.0002492605
HH.pl.vs.lnorm$test_statistic       # 3.48155


(TC.dislnorm$setXmin(TC.displ$getXmin()))
TC.dislnorm$setPars(estimate_pars(TC.dislnorm)) 
TC.pl.vs.lnorm <- compare_distributions(TC.dislnorm, TC.displ)
TC.pl.vs.lnorm$p_two_sided          # 0.278343
TC.pl.vs.lnorm$p_one_sided          # 0.1391715
TC.pl.vs.lnorm$test_statistic       # 1.084049


#############################################################################
# MODELING WITH POWER LAW AND BARABASI ALBERT
#############################################################################
new_window("Hip Hop", 6, 8)
par(mfrow=c(2,1))
plot_distribution_pl(HH.artists, title = "Hip Hop", geodesic = TRUE)

# No good...
new_window("Hip Hop Preferential Attachment", 12, 4)
par(mfrow=c(2,1))
HH.sample_pa <- sample_pa(vcount(HH.artists), directed=FALSE, 
                              pa.exp=0.2, m=1, zero.appeal = 5)
plot_distribution_pl(HH.sample_pa, title = "Hip Hop Preferential Attachment", geodesic = TRUE)


# Tring again for techno
new_window("Techno", 6, 8)
par(mfrow=c(2,1))
plot_distribution_pl(TC.artists, title = "Techno", geodesic = TRUE)

# Also no good
new_window("Techno Preferential Attachment", 12, 4)
par(mfrow=c(1,2))
HH.sample_pa <- sample_pa_age(vcount(TC.artists), directed=FALSE, pa.exp=2, m=1, aging.exp=-2)
plot_distribution_pl(TC.sample_pa, title = "Techno Preferential Attachment", geodesic = TRUE)


#############################################################################
# Communities
#############################################################################
# Hip Hop Cluster Louvain
HH.cl <- cluster_louvain(HH.artists)
modularity(HH.cl)                    # 0.8496378
length(HH.cl)                        # 2538
table(sizes(HH.cl))

V(HH.artists)$Cluster_Louvain <- membership(HH.cl)
assortativity_nominal(HH.artists, 
                      V(HH.artists)$Cluster_Louvain) # 0.867822

# Hip Hop Cluster Infomap
HH.info <- cluster_infomap(HH.artists)
modularity(HH.info)                  # 0.785544
length(HH.info)                      # 2895
V(HH.artists)$Cluster_Infomap <- membership(HH.info)

assortativity_nominal(HH.artists, 
                      V(HH.artists)$Cluster_Infomap) # 0.7725591

# Techno Cluster Louvain
TC.cl <- cluster_louvain(TC.artists)
modularity(TC.cl)                    # 0.9697976
length(TC.cl)                        # 1882
V(TC.artists)$Cluster_Louvain <- membership(TC.cl)
# Techno Cluster Infomap
TC.info <- cluster_infomap(TC.artists)
modularity(TC.info)                 # 0.9312459
length(TC.info)                     # 2060
V(TC.artists)$Cluster_Infomap <- membership(TC.info)


# Overlapping communities
HH.edges <- as_edgelist(HH.artists)
HH.lc <- getLinkCommunities(HH.edges, hcmethod = "average")

HH.cw <- getCommunityCentrality(HH.lc, type = "commweight")
top.cw <- head(sort(HH.cw, decreasing = TRUE), n = 10)
V(HH.artists)[as.numeric(names(top.cw))]$label
#[1] "DJ Khaled"       "Lil Wayne"       "Jay-Z"           "Rick Ross"      
#[5] "Lil' Jon"        "Snoop Dogg"      "Mase"            "Quavo"          
#[9] "Chris Brown (4)" "RZA"    

HH.comcon <- getCommunityConnectedness(HH.lc, conn = "conn")
(top.com <- head(sort(HH.comcon, decreasing=TRUE)))
#795     1063     1298     1147     1062     1126 
#64.44258 50.20314 49.29036 46.00433 40.34507 39.97996 

V(HH.artists)[as.numeric(getNodesIn(HH.lc, clusterids = 795))]$label
#[1] "GZA"               "RZA"               "Inspectah Deck"    "Method Man"       
#[5] "Nas"               "J-Love"            "Raekwon"           "Ghostface Killah" 
#[9] "Wu-Tang Clan"      "Ol' Dirty Bastard" "U-God"             "Masta Killa"      
V(HH.artists)[as.numeric(getNodesIn(HH.lc, clusterids = 1063))]$label
#[1] "Mary J. Blige"      "Snoop Dogg"         "Bobby Brown"        "Charlie Wilson"    
#[5] "Kardinal Offishall" "Damian Marley"      "Colby O'Donis"      "DJ Vadim"          
#[9] "The Electric"       "Yarah Bravo"        "Shaggy"             "Sway"              
#[13] "Maxi Priest"        "Two Fingers"       
V(HH.artists)[as.numeric(getNodesIn(HH.lc, clusterids = 1298))]$label
#[1] "Usher"           "Lil Wayne"       "Rick Ross"       "Chris Brown (4)"
#[5] "DJ Khaled"       "Young Jeezy"    

V(HH.artists)[as.numeric(getNodesIn(HH.lc, clusterids = 1147))]$label
#[1] "Q-Tip"                "A Tribe Called Quest" "Fugees"              
#[4] "Busta Rhymes"         "John Forte"           "Lauryn Hill"         
#[7] "Wyclef Jean"          "Pras Michel"          "Phife Dawg"          
#[10] "Ali Shaheed Muhammad" "Jarobi White"         "Kelis"               

V(HH.artists)[as.numeric(getNodesIn(HH.lc, clusterids = 1062))]$label
#[1] "Mary J. Blige"      "Pharrell Williams"  "Bobby Brown"        "Charlie Wilson"    
#[5] "Kardinal Offishall" "Damian Marley"      "Colby O'Donis"      "DJ Vadim"          
#[9] "The Electric"       "Yarah Bravo"        "Shaggy"             "Sway"              
#[13] "Maxi Priest"        "Two Fingers"       
> 



HH.comm <- reify_link_communities(HH.artists, HH.lc)
summary(HH.comm)



# Ego Networks for Hip Hop Only
(snoop_c <- V(HH.comm)[label == "Snoop Dogg"])
(nas_c <- V(HH.comm)[label == "Nas"])
(lil_jon_c <- V(HH.comm)[label == "Lil' Jon"])
(ghostface_c <- V(HH.comm)[label == "Ghostface Killah"])
(pharell_c <- V(HH.comm)[label == "Pharrell Williams"])

AoI_c <- c(snoop_c, nas_c, lil_jon_c, ghostface_c, pharell_c)

for(v in AoI){
  adj <- adjacent_vertices(HH.comm, v, mode = "all")[[1]]
  sc <- adj[comm_p == TRUE]
  print(degree(HH.comm, sc))
}




find_bridges <- function(g, v){
  cnames <- c("vertex", "n1", "n1_adj","n2","n2_adj")
  df <- data.frame(matrix(data = 0, nrow = 0, ncol = 5))
  colnames(df) <- cnames
  adj <- adjacent_vertices(g, v, mode = "all")[[1]][comm_p == FALSE]
  print(v)
  if(length(adj) > 1){
    pairs <- combn(adj, m = 2)
    
    for(i in 1:(length(pairs)/2)){
      n1 <- V(g)[pairs[1,i]]
      n2 <- V(g)[pairs[2,i]]
      n1.comms <- adjacent_vertices(g, n1, mode = "all")[[1]][comm_p == TRUE]
      n2.comms <- adjacent_vertices(g, n2, mode = "all")[[1]][comm_p == TRUE]
      mutual.comms <- intersection(n1.comms, n2.comms)
    
      if(length(mutual.comms) == 0 & length(n1.comms) > 0 & length(n2.comms) > 0){
        n1.adj <- length(adjacent_vertices(g, n1, mode = "all")[[1]][comm_p == FALSE])
        n2.adj <- length(adjacent_vertices(g, n2, mode = "all")[[1]][comm_p == FALSE])
      
        de <- data.frame(list(v$label, n1$label, n1.adj, n2$label, n2.adj))
        colnames(de) <- cnames
        df <- rbind(df, de)
      }
    }
  }
  return(df)
}




biggest_lc <- function(g, l_comms){
  sizes <- c()
  for(lc in l_comms){
    sizes <- c(sizes, length(adjacent_vertices(g, lc, mode = "all")[[1]]))
  }
  return(max(sizes))
}
#############################################################################
# ROBUSTNESS AND CASCADES
#############################################################################

# Robustness
new_window(width=8,height=4)
par(mfrow=c(1, 2))
HH.failure.metrics <- robustness_simulation(HH.artists,  attack = FALSE, cutoff = .9)
plot_robustness_results(HH.failure.metrics, "Attack on Hip Hop")


new_window(width=8,height=4)
par(mfrow=c(1, 2))
TC.failure.metrics <- robustness_simulation(TC.artists,  attack = TRUE, cutoff = .9)
plot_robustness_results(TC.failure.metrics, "Attack on Techno")

# SIR (SI)
HH.sir <- sir(HH.artists, beta= 0.25, gamma = 0, no.sim = 50)
plot(HH.sir, main = "Hip Hop SI Model")

TC.sir <- sir(TC.artists, beta= 0.25, gamma = 0, no.sim = 50)
plot(TC.sir, main = "Techno SI Model")

# Summarize and save hip hop
summary(HH.artists)
write_graph(HH.artists, "graphs/hip_hop_artists.graphml",
            format = "graphml")

# Summary and Save Techno
summary(TC.artists)
write_graph(HH.artists, "graphs/techno_artists_fixed.graphml",
            format = "graphml")
