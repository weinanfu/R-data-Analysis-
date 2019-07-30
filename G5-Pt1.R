library(igraph)
setwd("/Users/fuweinan/Desktop/Courses")
g <- read.graph("CA-AstroPh.txt", format="edgelist")
V(g)
E(g)
is_simple(g)
ng <- simplify(g, remove.multiple =TRUE, remove.loops=TRUE )
ng
is_simple(ng)

print(V(ng)[degree(ng)==max(degree(ng))]) #find the vertex with maximum degree 
degree(ng, v = 53214) #calculate the degree of this vertex
d <-diameter(ng, 
             directed = TRUE, 
             unconnected = TRUE, 
             weights = NULL)#the longest path is the diameter of the graph


largest_cliques(ng) #finds all largest cliques in the input graph

ego(ng, order = 1, nodes = 53214, mode = c("all", "out", "in"),
    mindist = 0)#find the neighboorhood vertexes of the vertex with maximum degree
power_centrality(ng, nodes = V(ng), loops = FALSE, exponent = 1,
                 rescale = FALSE, tol = 1e-07, sparse = TRUE)

mst(g,weights = graph_attr(ng,'weight')) #find Minimum spanning tree

is.connected(ng) #Is it connected?
no.clusters(ng) # How many components?
table(clusters(ng)$csize) # How big are these?
max(degree(ng, mode="in")) # Vertex degree
max(degree(ng, mode="out"))

plot(degree.distribution(ng, mode="in"), log="xy")max(degree(ng, mode="all"))# In-degree distribution
cl <- clusters(ng)
ng2 <- subgraph(ng, which(cl$membership == which.max(cl$csize)-1)-1)
summary(ng2)
graph.density(ng) # Density

# Generate random graph, fixed probability
g <- erdos.renyi.game(20, 0.3)
plot(g, layout=layout.fruchterman.reingold, vertex.label=NA, vertex.size=5)

g <- erdos.renyi.game(12, 0.25)
plot(g, layout=layout.fruchterman.reingold)
pa <- get.shortest.paths(g, 5, 9)[[1]]
# Number of islands
clusters(ng)$no

# Reciprocity of the graph
reciprocity(ng)

