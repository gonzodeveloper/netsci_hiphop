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
setwd("~/repo/netsci_hiphop/")
source('utilities.R')

# Load hip-hop artist-release edge list, make graph 
df <- read.csv("data/hip_hop_groups_release.csv")
HH <- graph.data.frame(df)

# Get a projection off the bipartite graph
V(HH)$type <- bipartite_mapping(HH)$type
HH.proj <- bipartite_projection(HH)
HH.artists <- HH.proj$proj1


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

# Summary and Save (notice that this single removal subtracted 22,986 edges from the projection)
summary(TC.artists)
write_graph(HH.artists, "graphs/techno_artists_fixed.graphml",
            format = "graphml")



#############################################################################
# RUNNING SIMPLE METRICS TO COMPARE EACH GRAPH
#############################################################################

# Number of verticies N
(HH.N <- length(V(HH.artists)))        # 10117
(TC.N <- length(V(TC.artists)))        # 6078
# Average Degree <k>
(HH.k <- mean(degree(HH.artists)))     # 2.736977
(TC.k <- mean(degree(TC.artists)))     # 1.733465
# Geodesic Distances
mean_distance(HH.artists)              # 5.6226
mean_distance(TC.artists)              # 9.3225

# Calculating Regimes
log(HH.N)             # With a random graph modeling these networks we would
log(TC.N)             # expect both graphs to be in supercritical regime

# Number of Components
length(sort(components(HH.artists)$csize))  # 2499
length(sort(components(TC.artists)$csize))  # 1859

# Largest Component
max(components(HH.artists)$csize)            # 3856
max(components(TC.artists)$csize)            # 1538


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
V(HH.artists)$authority <- authority_score(HH.artists)$vector
V(HH.artists)$hub       <- hub_score(HH.artists)$vector
V(HH.artists)$betweenness <- betweenness(HH.artists, normalized=TRUE)
V(HH.artists)$closeness <- closeness(HH.artists, normalized=TRUE)

V(TC.artists)$degree <- degree(TC.artists)
V(TC.artists)$strength <-strength(TC.artists)
V(TC.artists)$eigen_centrality <- eigen_centrality(TC.artists)$vector
V(TC.artists)$page_rank <- page_rank(TC.artists)$vector
V(TC.artists)$authority <- authority_score(TC.artists)$vector
V(TC.artists)$hub       <- hub_score(TC.artists)$vector
V(TC.artists)$betweenness <- betweenness(TC.artists, normalized=TRUE)
V(TC.artists)$closeness <- closeness(TC.artists, normalized=TRUE)

# Print results for each
V(HH.artists)[order(V(HH.artists)$degree, decreasing=TRUE)[1:10]]$name
#[1] "Snoop Dogg"        "Lil Wayne"         "Jay-Z"            
#[4] "Busta Rhymes"      "Pharrell Williams" "Kanye West"       
#[7] "Nas"               "50 Cent"           "Akon"             
#[10] "Lil' Jon" 
V(HH.artists)[order(V(HH.artists)$strength, decreasing=TRUE)[1:10]]$name
#[1] "Snoop Dogg"       "Lil Wayne"        "50 Cent"         
#[4] "Method Man"       "RZA"              "Ghostface Killah"
#[7] "Jay-Z"            "GZA"              "Lil' Jon"        
#[10] "Raekwon" 
topnv(HH.artists, V(HH.artists)$eigen_centrality)$name
#[1] "Ghostface Killah"  "Raekwon"           "Method Man"       
#[4] "RZA"               "Inspectah Deck"    "GZA"              
#[7] "Ol' Dirty Bastard" "Wu-Tang Clan"      "Masta Killa"      
#[10] "U-God" 
topnv(HH.artists, V(HH.artists)$page_rank)$name
#[1] "Snoop Dogg"        "Lil Wayne"         "Jay-Z"            
#[4] "Lil' Jon"          "50 Cent"           "Pharrell Williams"
#[7] "Busta Rhymes"      "Kanye West"        "Method Man"       
#[10] "Nas"  
topnv(HH.artists, V(HH.artists)$authority)$name
#[1] "Ghostface Killah"  "Raekwon"           "Method Man"       
#[4] "RZA"               "Inspectah Deck"    "GZA"              
#[7] "Ol' Dirty Bastard" "Wu-Tang Clan"      "U-God"            
#[10] "Masta Killa" 
topnv(HH.artists, V(HH.artists)$hub)$name
#[1] "Ghostface Killah"  "Raekwon"           "Method Man"       
#[4] "RZA"               "Inspectah Deck"    "GZA"              
#[7] "Ol' Dirty Bastard" "Wu-Tang Clan"      "U-God"            
#[10] "Masta Killa" 
topnv(HH.artists, V(HH.artists)$betweenness)$name
#[1] "Snoop Dogg"        "Jay-Z"             "Nas"              
#[4] "Busta Rhymes"      "KRS-One"           "Lil Wayne"        
#[7] "Pharrell Williams" "Kanye West"        "Eminem"           
#[10] "Mos Def"
topnv(HH.artists, V(HH.artists)$closeness)$name
#[1] "Snoop Dogg"        "Pharrell Williams" "Busta Rhymes"     
#[4] "Nas"               "T.I."              "50 Cent"          
#[7] "Lil Wayne"         "Kanye West"        "Ludacris"         
#[10] "Mary J. Blige"  



V(TC.artists)[order(V(TC.artists)$degree, decreasing=TRUE)[1:10]]$name
#[1] "Adam Beyer"           "D.A.V.E. The Drummer"
#[3] "Cari Lekebusch"       "Mark Broom"          
#[5] "Jack Wax"             "Ant"                 
#[7] "Sterling Moss"        "Ben Sims"            
#[9] "Guy McAffer"          "Chris Liberator" 
V(TC.artists)[order(V(TC.artists)$strength, decreasing=TRUE)[1:10]]$name
#[1] "Adam Beyer"           "Mark Broom"          
#[3] "D.A.V.E. The Drummer" "Sterling Moss"       
#[5] "Jack Wax"             "Ant"                 
#[7] "Ben Sims"             "Cari Lekebusch"      
#[9] "Benji303"             "Chris Liberator"  
topnv(TC.artists, V(TC.artists)$eigen_centrality)$name
#[1] "Adam Beyer"     "Cari Lekebusch" "Patrice Scott" 
#[4] "Muslimgauze"    "C_C"            "Genghis (2)"   
#[7] "Andy Vaz"       "Metek"          "Underspreche"  
#[10] "powwowW" 
topnv(TC.artists, V(TC.artists)$page_rank)$name
#[1] "Mark Broom"           "D.A.V.E. The Drummer"
#[3] "Adam Beyer"           "Ben Sims"            
#[5] "Ant"                  "Guy McAffer"         
#[7] "Sterling Moss"        "Alex Calver"         
#[9] "Gary Beck"            "Jack Wax" 
topnv(TC.artists, V(TC.artists)$authority)$name
#[1] "Adam Beyer"     "Cari Lekebusch" "Muslimgauze"   
#[4] "Patrice Scott"  "Andy Vaz"       "Mentalic"      
#[7] "Alex Hasten"    "powwowW"        "Micro Naps"    
#[10] "Underspreche" 
topnv(TC.artists, V(TC.artists)$hub)$name
#[1] "Adam Beyer"     "Cari Lekebusch" "Muslimgauze"   
#[4] "Patrice Scott"  "Andy Vaz"       "Mentalic"      
#[7] "Alex Hasten"    "powwowW"        "Micro Naps"    
#[10] "Underspreche"
topnv(TC.artists, V(TC.artists)$betweenness)$name
#[1] "Mark Broom"           "D.A.V.E. The Drummer"
#[3] "Pounding Grooves"     "Ben Sims"            
#[5] "Ant"                  "DJ Ogi"              
#[7] "Green Velvet"         "Adam Beyer"          
#[9] "Mr. G"                "Bad Boy Pete"  
topnv(TC.artists, V(TC.artists)$closeness)$name
#[1] "Mark Broom"           "Ben Sims"            
#[3] "Pounding Grooves"     "D.A.V.E. The Drummer"
#[5] "Mr. G"                "Markus Suckut"       
#[7] "Chris Liebing"        "Edit Select"         
#[9] "Davide Squillace"     "Terry Brookes" 


# Ego Networks for Hip Hop Only
(snoop <- V(HH.artists)[which(V(HH.artists)$name=="Snoop Dogg")])
(nas <- V(HH.artists)[which(V(HH.artists)$name=="Nas")])
(jay_z <- V(HH.artists)[which(V(HH.artists)$name=="Jay-Z")])
(ghostface <- V(HH.artists)[which(V(HH.artists)$name=="Ghostface Killah")])
(pharell <- V(HH.artists)[which(V(HH.artists)$name=="Pharrell Williams")])


HH.snoop <- make_and_plot_ego_graph(HH.artists, snoop, 3)
HH.nas <- make_and_plot_ego_graph(HH.artists, nas, 3)
HH.jay_z <- make_and_plot_ego_graph(HH.artists, jay_z, 3)
HH.ghostface <- make_and_plot_ego_graph(HH.artists, ghostface, 3)
HH.pharell <- make_and_plot_ego_graph(HH.artists, pharell, 3)

write_graph(HH.snoop, "snoop.graphml", format = "graphml")
write_graph(HH.nas, "nas.graphml", format = "graphml")
write_graph(HH.jay_z, "jay-z.graphml", format = "graphml")
write_graph(HH.ghostface, "ghostface.graphml", format = "graphml")
write_graph(HH.pharell, "pharell.graphml", format = "graphml")



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


# We can see that neither of the networks' degree distributions are particularly well 
# modeled by Poisson, Exponential, Log Normal or Power Law Models
new_window()
plot_discrete_distributions("Hip Hop Distribution Comparison", HH.disexp, HH.dislnorm, HH.displ, HH.dispois)
new_window()
plot_discrete_distributions("Teechno Distribution Comparison", HH.disexp, HH.dislnorm, HH.displ, HH.dispois)

#############################################################################
# MODELING WITH POWER LAW AND BARABASI ALBERT
#############################################################################
new_window("Hip Hop", 12, 4)
par(mfrow=c(1,2))
plot_distribution_pl(HH.artists, title = "Hip Hop", geodesic = TRUE)


new_window("Hip Hop Preferential Attachment", 12, 4)
par(mfrow=c(1,2))
HH.sample_pa <- sample_pa_age(vcount(HH.artists), directed=FALSE, pa.exp=0.8, m=1)
plot_distribution_pl(HH.sample_pa, title = "Hip Hop Preferential Attachment", geodesic = TRUE)


# Tring again for techno
new_window("Techno", 12, 4)
par(mfrow=c(1,2))
plot_distribution_pl(TC.artists, title = "Techno", geodesic = TRUE)


new_window("Techno Preferential Attachment", 12, 4)
par(mfrow=c(1,2))
HH.sample_pa <- sample_pa_age(vcount(TC.artists), directed=FALSE, pa.exp=2, m=1, aging.exp=-2)
plot_distribution_pl(TC.sample_pa, title = "Techno Preferential Attachment", geodesic = TRUE)


#############################################################################
# CLUSTERING
#############################################################################
# Hip Hop Cluster Louvain
HH.cl <- cluster_louvain(HH.artists)
modularity(HH.cl)                    # 0.8496378
length(HH.cl)                        # 2538
V(HH.artists)$Cluster_Louvain <- membership(HH.cl)

# Hip Hop Cluster Infomap
HH.info <- cluster_infomap(HH.artists)
modularity(HH.info)                  # 0.785544
length(HH.info)                      # 2895
V(HH.artists)$Cluster_Infomap <- membership(HH.info)

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

#############################################################################
# ROBUSTNESS AND CASCADES
#############################################################################

HH.components <- components(HH.artists)
big <- which(HH.components$csize == max(HH.components$csize))
HH.giant.nodes <- V(HH.artists)[which(HH.components$membership == big)]
HH.giant <- induced_subgraph(HH.artists, HH.giant.nodes) 
summary(HH.giant)


TC.components <- components(TC.artists)
big <- which(TC.components$csize == max(TC.components$csize))
TC.giant.nodes <- V(TC.artists)[which(TC.components$membership == big)]
TC.giant <- induced_subgraph(TC.artists, TC.giant.nodes) 
summary(TC.giant)

HH.failure.metrics <- robustness_simulation(HH.artists,  attack = TRUE, cutoff = .9)
plot_robustness_results(HH.failure.metrics, "Attack on Hip Hop")

HH.giant.failure.metrics <- robustness_simulation(HH.giant, attack = TRUE, cutoff = .9)
plot_robustness_results(HH.giant.failure.metrics, "Attack on Hip Hop Main")

TC.failure.metrics <- robustness_simulation(TC.artists,  attack = TRUE, cutoff = .9)
plot_robustness_results(TC.failure.metrics, "Attack on Techno")

TC.giant.failure.metrics <- robustness_simulation(TC.giant, attack = TRUE, cutoff = .9)
plot_robustness_results(TC.giant.failure.metrics, "Attack on Techno Main")


# Summarize and save hip hop
summary(HH.artists)
write_graph(HH.artists, "graphs/hip_hop_artists.graphml",
            format = "graphml")

# Summary and Save Techno
summary(TC.artists)
write_graph(HH.artists, "graphs/techno_artists_fixed.graphml",
            format = "graphml")
