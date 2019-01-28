## SNA of terrorist organizations
##load libraries

## load file
terrorgroups <- read.csv(file.choose(),header=TRUE,sep=",") 


# Step 1: included core variables = group, state sponsored, falalities, org age
terrorgroups[,c(5)]

e101 <- terrorgroups[,c(5)]
e102 <- as.matrix(e101)
V(g)$size <- e102

e5 <- terrorgroups[,c(1,2,5,6)]
e6 <- as.matrix(e5)
df <- graph.data.frame(e6,directed=TRUE)
plot(df)
summary(df)
class(df)

plot.igraph(df,
            layout=layout_with_fr(df),
            margin = -.20,
            vertex.label=V(df)$name,
            vertex.label.color="black",
            vertex.label.family = "sans",
            vertex.label.cex=.2,
            vertex.frame.color = NA,
            vertex.size=V(g)$size*0.01, 
            vertex.color=V(df)$color,
            edge.color=E(df)$color,
            edge.curved=FALSE,
            edge.arrow.size=.3,
            edge.arror.color=E(df)$color, main="Terrorist Groups and State Sponsorship")

summary(e5[,c(2)]) # 363 no, 32 yes
summary(e5[,c(5)])

##step 2: 
### Small-world detection, clusters, centrality measures
###R.Q. 1: are there clusters within the terrorist groups? which group is the most central?

groupscl <- as.matrix(terrorgroups[,c(1,9)])
head(groupscl)
g5 <- graph.edgelist(groupscl,directed=TRUE)
g6 <- graph.edgelist(groupscl,directed=FALSE) ##UNDIRECTED

# Confirming (igraph) object
class(g5) #igraph
summary(g5)
# View all of the nodes
V(g5)
# View all of the edgewise mappings
E(g5)

plot.igraph(g5,
            layout=layout_with_fr(g5),
            margin = -.15,
            vertex.label=V(g5)$name,
            vertex.label.color="black",
            vertex.label.family = "sans",
            vertex.label.cex=.2,
            vertex.frame.color = NA,
            vertex.size=3,
            vertex.color=V(g5)$color,
            edge.color=E(g5)$color,
            edge.curved=FALSE,
            edge.arrow.size=.3,
            edge.arror.color=E(g5)$color, main="Alliance Connections")
## variable degree has the count of alliance connections, plot shows the groups and number of alliances but no ties??

## step 3: Affiliation Network
#create data frame from 8 variables
t <- read.csv(file.choose(),header=TRUE,sep=",") 


a <- t[,c(1,10,11,12,13,14,15,18)]
a2 = as.data.frame(a)
summary(a2)
head(a2)
tail(a2)

#graph data 
g7 = graph.incidence(a2, directed = FALSE)
V(g7)$label =V(g7)$name 
g7
plot(g7) #Check plot

# Melt data
library(reshape2)
m <- melt(a2, id=c(1))
head(m)

m4 = data.frame(m)
class(m4)
 
g12 = graph.incidence(m4, directed = FALSE)
g12
plot(g12, vertex.size=3, vertex.label=m4$variable)

#filter to only 1s

m2 <- m[m$value == '1',]
head(m2)

m3 <- data.frame(m2)
class(m3)

g11 <- graph_from_data_frame(m3)
g11
plot(g11, vertex.size=3, vertex.label=NA)
plot(g11, vertex.size=3, vertex.label=m2$variable)
plot.igraph(g11,
            layout=layout_with_fr(g11),
            margin = -.30,
            vertex.label=m2$variable,
            vertex.label.color="black",
            vertex.label.family = "sans",
            vertex.label.cex=.3,
            vertex.frame.color = NA,
            vertex.size=.3,
            edge.arrow.size=.3,
             main="Affiliation Network")


#check the data if bipartite
E(g11)  #3160 edges
V(g11)  #403 vertices
V(g11)$type  #null ??
E(g7)$name  
get.incidence(g11) #not bipartite
graph.density(g11)==graph.density(g11)




