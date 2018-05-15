# **Network Analysis of the Hip Hop Community**

## Kyle Hart - M.S. Candidate - University of Hawaii at Manoa

### Abstract

	In the early 2000’s as hip hop was entering the popular zeitgeist there was an ongoing joke about how many artists would be featured on a single track. While collaborations were not unknown to other genres of music—occasionally 90’s rockers would team up to form “supergroups” like Audioslave or the Foo Fighters—rappers took the practice to a whole new level. Eight or nine artists could be featured on a single track, a couple dozen might be credited to a single album. There were even some mercenaries with no career of their own own to speak of, but would gain fame by appearing on hundreds of others’ tracks—perhaps the most notorious of these was Lil Jon  who would show up on singles just to scream “Yeah!” a couple times. 
    
    If we see these artists and their collaborations as nodes and edges in a network we can run this graph through a gauntlet of metrics and models so that we might compare it to other commonly studied social networks and even to other genres of music. With this analysis we will delve into what makes this particular community unique and what processes are in play that contribute to this distinction.
    
### The Data

	In November 2000 Portland based programmer Kevin Lewandowski launched a crowdsourced database called Discogs1. Currently this is the largest known public database of meta-data on commercial and non-commercial audio releases, containing information on nearly 10 million releases from over 5 million artists. The data can be accessed via a XML-based RESTful API or by direct download. 
    
    For this project we downloaded the most recent database release from April 2004 and extracted the desired data with a Python based SAX parser. 

    Because the each release is stored as an XML document with an entry listing the contributing artists, it was necessary to first construct a bipartite graph from from artists to releases, then to extract our collaboration network from the projection of this graph. Additionally, it should be noted that only a subset of the database was used, namely we were only interested in releases with ‘Hip Hop’ as one of their genre tags, and only those that had more than one artist credited. This means that it is possible, even likely, that there are some rappers left out that released only by themselves. The decision to leave out these isolates was motivated by the fact that we are interested in the community, which canonically disqualifies isolates in the same sense that backyard power generators are excluded from the analysis of a power grid. 
    
    Moreover, since many of the releases we credited to groups the decision was made to break these groups down into their actual members. This was necessary to get a full picture of the network due to the sheer frequency of which artists would work outside of their groups and due to the very nature of group productions. Unlike most pop or rock groups, a hip-hop group production has the same structure as one produced by multiple solo artists—each verse is given to an individual rapper, who will often write their lyrics independent of their collaborators input. 
    
    Finally, to get a genre for which we could run comparisons we went through the same process to derive a graph for ‘Techno’ artists. The motivation of this choice of genre is explained below. 
    
### Background

	A career of a hip hop artist follows a fairly standard tract. As with other genres the young rapper generally starts off locally, preforming at small shows independently or along side others long before they put out their first production. However, unlike other genres where these young artists are scouted by labels then signed into singles and album deals independently, budding rappers will often group up with other local MCs and DJs to increase their own notoriety before taking off on a solo career. Even in cases where local rappers are scouted by labels, they are often first debuted by recording tracks or verses on the albums of more prominent artists. 
    
    By contrast, techno artists follow a more traditional path. They start local, increase their following on the club-scene, then go on to release labeled production albums. Structurally the two genres could not be more different. While on a single track it is simple for a collection of MCs to alternate verses over a per-recorded beat, it is much more difficult for multiple techno DJs to contribute to same track. In fact, most collaborations for the latter that we are reviewing are over entire albums, rather then co-authorship of individual songs that we see from the former.
    
    This domain information should help inform some of the basic metrics on the two network graphs and assist in the parameterization of our models.
    
