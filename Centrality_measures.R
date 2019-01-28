#SNA measures of centrality
#Stanford dataset
# Data Source: https://snap.stanford.edu/data/gemsec-Deezer.html
# Task: measures of centrality - betwenness, closeness and degree

#load library/install packages
install.packages("rjson")
library(jsonlite)
library(rjson)
library(igraph)
library(reshape2)
library(dplyr)

#load in the nodes and edges 
# Nodes represent the users and edges are the mutual friendships. 
# We reindexed the nodes in order to achieve a certain level of anonimity. 
# The csv files contain the edges -- nodes are indexed from 0. 
# 3 countries: RO = Romania, HR = Croatia, HU = Hungary

eRO <- read.csv("~/Desktop/RO_edges.csv")
eHR <- read.csv("~/Desktop/HR_edges.csv")
eHU <- read.csv("~/Desktop/HU_edges.csv")

#load json files and transform to data frame
# the json files contain the genre preferences of users -- each key is a user id, 
# the genres loved are given as lists. Genre notations are consistent across users. 
# In each dataset users could like 84 distinct genres. 
# Liked genre lists were compiled based on the liked song lists.

#RO attributes
# Load in the data
json_data2 <- fromJSON("~/Desktop/RO_genres.json")

# Check the user list
names(json_data2)

# Typically the first variable represents the row names or are simply an enumeration
json_data2$`0` <- rownames(json_data2) 

# original frame was rectangular (n_rows!= n_cols). This basically meant that you 
# could not make a data.frame out of the column- and rownames since each column in a data.frame 
# must be the same length.
melted_list <- melt(json_data2)

melted_list

#HU attributes
# Load in the data
json_data3 <- fromJSON("~/Desktop/HU_genres.json")

# Check the user list
names(json_data3)

# Typically the first variable represents the row names or are simply an enumeration
json_data3$`0` <- rownames(json_data3) 

# Your original frame was rectangular (n_rows!= n_cols). This basically meant that you 
# could not make a data.frame out of the column- and rownames since each column in a data.frame 
# must be the same length.
melted_listHU <- melt(json_data3)

melted_listHU


#HR attributes
# Load in the data
json_data4 <- fromJSON("~/Desktop/HR_genres.json")

# Check the user list
names(json_data4)

# Typically the first variable represents the row names or are simply an enumeration
json_data4$`13357` <- rownames(json_data4) 

# Your original frame was rectangular (n_rows!= n_cols). This basically meant that you 
# could not make a data.frame out of the column- and rownames since each column in a data.frame 
# must be the same length.
melted_listHR <- melt(json_data4)

melted_listHR

#combine attributes with edges/nodes (DIDNT WORK ERROR)
gRO <- graph_from_data_frame(d=eRO, vertices = melted_list, directed=TRUE) #error duplicate vertex names

RO <- cbind(eRO, melted_list) #error, arguments imply differing number of rows: 125826, 252123
HR <- cbind(eHR, melted_listHR) # error, different number of rows for both data

rbind(eRO, melted_list)

#working with edges only w/o attributes

#Romania data
g <- graph_from_data_frame(d=eRO, directed=TRUE)

plot(g) #directed network

class(g)


#view nodes/edges
V(g) # nodes - 41773 vertices
V(g)$name # names of each node
vertex_attr(g) # all attributes of the nodes - no attributes yet!!
E(g) # edges - 125826 edges
E(g)$weight # weights for each edge - NULL
edge_attr(g) # all attributes of the edges - NULL 
g[] # adjacency matrix
g[1,] # first row of adjacency matrix

par(mar=c(0,0,0,0))

plot(g,layout = layout.fruchterman.reingold(g),
     margin = -.10,
     vertex.color = "grey", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = .75, # change size of labels to 75% of original size
     vertex.size = .75,
     edge.curved=.25, # add a 25% curve to the edges
     edge.color="grey20",
     edge.arrow.size = .1) # change edge color to grey
  

#plot a subset

gs <- sample_n(eRO, 100)
gs1 <- graph_from_data_frame(d=gs, directed=TRUE)
plot.igraph(gs1,
            vertex.color = "grey", # change color of nodes
            vertex.label.color = "black", # change color of labels
            vertex.label.cex = .75, # change size of labels to 75% of original size
            edge.curved=.10, # add a 25% curve to the edges
            edge.color="grey20")


#Centrality measures

#Degree Centrality 
# The most basic measure is degree, the number of adjacent edges to each node. It is often considered a
# measure of direct influence. The unique number of characters that each
# character is interacting with.

# ROMANIA DATA
sort(degree(g))

sort(strength(g))

degree = degree(g, mode="all")

degree

#Closeness centrality
# Closeness measures how many steps are required to access every other node from a given node. It’s a
# measure of how long information takes to arrive (who hears news first?). 
# Higher values mean less centrality

closeness = closeness(g, vids = V(g), mode = c("total"),
                      weights = NULL, normalized = FALSE)
closeness

#Betweenness Centrality
# Betweenness measures brokerage or gatekeeping potential. It is (approximately) the number of shortest
# paths between nodes that pass through a particular node.

betweenness = betweenness(g, directed=T, weights=NA, normalized = T)
betweenness

#display them in a single table

df.prom2 = data.frame(degree = degree(g), closeness = closeness(g), betweenness = betweenness(g))
df.promsort = df.prom2 [order(-df.prom2$degree),] #sort the table

write.csv(df.promsort, file="centmeasures.csv", row.names = FALSE, col.names = FALSE)

#network communities
components(g) # only 1 community, size = 41773

giant <- decompose(g)[[1]]

cluster_walktrap(giant) # groups: 2554, mod: 0.64

#HUNGARY DATA
# centrality measures

gHU <- graph_from_data_frame(d=eHU, directed=TRUE)

plot(gHU) #directed network

class(gHU) #igraph

#view nodes/edges
V(gHU) # nodes - 47538 vertices
V(gHU)$name # names of each node # nos from 0
vertex_attr(gHU) # all attributes of the nodes - no attributes yet!!
E(gHU) # edges - 222887 edges
E(gHU)$weight # weights for each edge - NULL
edge_attr(gHU) # all attributes of the edges - NULL 
gHU[] # adjacency matrix
gHU[1,] # first row of adjacency matrix

sort(degree(gHU))

sort(strength(gHU))

#degree centrality
degree2 = degree(gHU, mode="all")

degree2

# closeness
closeness2 = closeness(gHU, vids = V(gHU), mode = c("total"),
                      weights = NULL, normalized = FALSE)
closeness2

# betweeness
betweenness2 = betweenness(gHU, directed=T, weights=NA, normalized = T)

betweenness2 #NAs in all nodes, warning message: NAs produced by integer overflow

#display them in a single table

df.prom3 = data.frame(degree2 = degree(gHU), closeness2 = closeness(gHU), betweenness2 = betweenness(gHU))
df.promsort2 = df.prom3 [order(-df.prom3$degree),] #sort the table

write.csv(df.promsort2, file="centmeasuresHU.csv", row.names = FALSE, col.names = FALSE)

#network communities
components(gHU) # only 1 community, size = 47538

giant2 <- decompose(gHU)[[1]]

cluster_walktrap(giant2) # groups: 910, mod: 0.58

#CROATIA DATA
# centrality measures

gHR <- graph_from_data_frame(d=eHR, directed=TRUE)

plot(gHR) #directed network

class(gHR) #igraph

#view nodes/edges
V(gHR) # nodes - 54573 vertices
V(gHR)$name # names of each node # nos from 0
vertex_attr(gHR) # all attributes of the nodes - no attributes yet!!
E(gHR) # edges - 498202 edges
E(gHR)$weight # weights for each edge - NULL
edge_attr(gHR) # all attributes of the edges - NULL 
gHR[] # adjacency matrix
gHR[1,] # first row of adjacency matrix

sort(degree(gHR))

sort(strength(gHR))

#degree centrality
degree3 = degree(gHR, mode="all")

degree3

#closeness
closeness3 = closeness(gHR, vids = V(gHR), mode = c("total"),
                       weights = NULL, normalized = FALSE)
closeness3

#betwenness
betweenness3 = betweenness(gHR, directed=T, weights=NA, normalized = T)

betweenness3

#display them in a single table

df.prom4 = data.frame(degree3 = degree(gHR), closeness3 = closeness(gHR), betweenness3 = betweenness(gHR))
df.promsort3 = df.prom4 [order(-df.prom4$degree),] #sort the table

write.csv(df.promsort3, file="centmeasuresHR.csv", row.names = FALSE, col.names = FALSE)

#network communities
components(gHR) # only 1 community, size = 41773

giant3 <- decompose(gHR)[[1]]

cw <- cluster_walktrap(giant3) # groups: 2554, mod: 0.64

# Community detection: small-world communities

plot (cw, gHR)
plot(cw, gHR,vertex.label.cex=0.5, vertex.label.color="black", vertex.label.font=0.5, vertex.label.dist=0,
     edge.color = "gray80", main="Small-World Communities")

#************************************************************
#subset - 

g2 <- subgraph.edges(g, E(g)[inc(V(g)[name %in% 0:1000])]) 
plot(g2,layout=layout_with_fr(g2),
     margin = -.10,
     vertex.label=V(g2)$name,
     vertex.label.color="black",
     vertex.label.family = "sans",
     vertex.label.cex=.2,
     vertex.frame.color = NA,
     vertex.size=2,
     vertex.color=V(g2)$color,
     edge.curved=FALSE,
     edge.arrow.size=.2,
     edge.arror.color=E(g2)$color)


top1000 = eRO %>%
  select(node_1, node_2) %>%
  arrange(desc(node_2)) %>%
  distinct(node_1)


