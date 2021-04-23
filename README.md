# networkAnlaysiswithNodesandEdges
netwwork Anlaysis with Nodes and Edges 
nodes <- read.csv("C:/Users/rengl/Documents/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("C:/Users/rengl/Documents/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
head(nodes)
head(links)
library(igraph)
net <- graph_from_data_frame(d=links,directed=T) 
net

E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media # Vertex attribute "media"

# Find nodes and edges by attribute:
# (that returns oblects of type vertex sequence/edge sequence)
V(net)[media=="BBC"]
E(net)[type=="mention"]

# You can also examine the network matrix directly:
net[1,]
net[5,7]

# Get an edge list or a matrix:
as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight")

# Or data frames describing nodes and edges:
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")

plot(net) # not a pretty picture!

net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net, edge.arrow.size=.4,vertex.label=NA)


nodes2 <- read.csv("C:/Users/rengl/Documents/Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("C:/Users/rengl/Documents/Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)
head(nodes2)
head(links2)

links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)

head(nodes2)
head(links2)

net2 <- graph_from_incidence_matrix(links2)
table(V(net2)$type)

# Plot with curved edges (edge.curved=.1) and reduce arrow size:
# Note that using curved edges will allow you to see multiple links
# between two nodes (e.g. links going in either direction, or multiplex links)
plot(net, edge.arrow.size=.4, edge.curved=.1)

# Set edge color to light gray, the node & border color to orange 
# Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$media, vertex.label.color="black")

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "plum"

# We can even set the network layout:
graph_attr(net, "layout") <- layout_with_lgl
plot(net) 

plot(net, edge.color="orange", vertex.color="gray50")

plot(net) 
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")

edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]

plot(net, edge.color=edge.col, edge.curved=.1) 
#
net.bg <- sample_pa(100) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)
#

net.bg <- sample_pa(100) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)
plot(net.bg, layout=layout_randomly)

# Randomly placed vertices
l <- layout_randomly(net.bg)
plot(net.bg, layout=l)

# 3D sphere layout
l <- layout_on_sphere(net.bg)
plot(net.bg, layout=l)

par(mfrow=c(2,2), mar=c(0,0,0,0))   # plot four figures - 2 rows, 2 columns
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=l)
plot(net.bg, layout=l)

#
l <- layout_with_fr(net.bg)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

par(mfrow=c(2,2), mar=c(0,0,0,0))
plot(net.bg, rescale=F, layout=l*0.4)
plot(net.bg, rescale=F, layout=l*0.6)
plot(net.bg, rescale=F, layout=l*0.8)
plot(net.bg, rescale=F, layout=l*1.0)

l <- layout_with_fr(net.bg, dim=3)
plot(net.bg, layout=l)

l <- layout_with_kk(net.bg)
plot(net.bg, layout=l)

l <- layout_with_graphopt(net.bg)
plot(net.bg, layout=l)
plot(net.bg, layout=layout_with_lgl)

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(3,3), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }

#
ggraph(net,  layout = 'lgl') +
  geom_edge_arc(color="gray", curvature=0.3) +            
  geom_node_point(color="orange", aes(size = audience.size)) +     
  geom_node_text(aes(label = media), size=3, color="gray50", repel=T) +
  theme_void()

#

# The charge parameter below changes node repulsion:
l1 <- layout_with_graphopt(net.bg, charge=0.02)
l2 <- layout_with_graphopt(net.bg, charge=0.00000001)

par(mfrow=c(1,2), mar=c(1,1,1,1))
plot(net.bg, layout=l1)
plot(net.bg, layout=l2)
######

E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media # Vertex attribute "media"

# Find nodes and edges by attribute:
# (that returns oblects of type vertex sequence/edge sequence)
V(net)[]
E(net)[]

# You can also examine the network matrix directly:
net[1,]
net[5,7]
# Get an edge list or a matrix:
as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight")

# Or data frames describing nodes and edges:
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")
plot(net) # not a pretty picture!

net <- simplify(net, remove.multiple = F, remove.loops = T) 

plot(net, edge.arrow.size=.4,vertex.label=NA)

links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)

plot(net) 
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(3,3), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }


hist(links$weight)
mean(links$weight)
sd(links$weight)

cut.off <- mean(links$weight) 
net.sp <- delete_edges(net, E(net)[weight<cut.off])
plot(net.sp, layout=layout_with_kk) 


par(mfrow=c(1,2))

# Community detection (by optimizing modularity over partitions):
clp <- cluster_optimal(net)
class(clp)

# Community detection returns an object of class "communities" 
# which igraph knows how to plot: 
plot(clp, net)

# We can also plot the communities without relying on their built-in plot:
V(net)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])

dev.off()

#
dist.from.NYT <- distances(net, v=V(net)[media=="NY Times"], 
                           to=V(net), weights=NA)

# Set colors to plot the distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]

plot(net, vertex.color=col, vertex.label=dist.from.NYT, edge.arrow.size=.6, 
     vertex.label.color="white")

news.path <- shortest_paths(net, 
                            from = V(net)[media=="MSNBC"], 
                            to  = V(net)[media=="New York Post"],
                            output = "both") # both path nodes and edges

# Generate edge color variable to plot the path:
ecol <- rep("gray80", ecount(net))
ecol[unlist(news.path$epath)] <- "orange"
# Generate edge width variable to plot the path:
ew <- rep(2, ecount(net))
ew[unlist(news.path$epath)] <- 4
# Generate node color variable to plot the path:
vcol <- rep("gray40", vcount(net))
vcol[unlist(news.path$vpath)] <- "gold"

plot(net, vertex.color=vcol, edge.color=ecol, 
     edge.width=ew, edge.arrow.mode=0)

inc.edges <- incident(net,  V(net)[media=="Wall Street Journal"], mode="all")

# Set colors to plot the selected edges.
ecol <- rep("gray80", ecount(net))
ecol[inc.edges] <- "orange"
vcol <- rep("grey40", vcount(net))
vcol[V(net)$media=="Wall Street Journal"] <- "gold"
plot(net, vertex.color=vcol, edge.color=ecol)

neigh.nodes <- neighbors(net, V(net)[media=="Wall Street Journal"], mode="out")

# Set colors to plot the neighbors:
vcol[neigh.nodes] <- "#ff9d00"
plot(net, vertex.color=vcol)

par(mfrow=c(1,2))
plot(net, mark.groups=c(1,4,5,8), mark.col="#C5E5E7", mark.border=NA)

# Mark multiple groups:
plot(net, mark.groups=list(c(1,4,5,8), c(15:17)), 
     mark.col=c("#C5E5E7","#ECD89A"), mark.border=NA)


dev.off()

tkid <- tkplot(net) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
plot(net, layout=l)


head(nodes2)
head(links2)

net2
plot(net2, vertex.label=NA)

dev.off()


# Media outlets are blue squares, audience nodes are orange circles:
V(net2)$color <- c("steel blue", "orange")[V(net2)$type+1]
V(net2)$shape <- c("square", "circle")[V(net2)$type+1]

# Media outlets will have name labels, audience members will not:
V(net2)$label <- ""
V(net2)$label[V(net2)$type==F] <- nodes2$media[V(net2)$type==F] 
V(net2)$label.cex=.6
V(net2)$label.font=2

plot(net2, vertex.label.color="white", vertex.size=(2-V(net2)$type)*8) 

plot(net2, vertex.label=NA, vertex.size=7, layout=layout.bipartite) 

plot(net2, vertex.shape="none", vertex.label=nodes2$media,
     vertex.label.color=V(net2)$color, vertex.label.font=2, 
     vertex.label.cex=.6, edge.color="gray70",  edge.width=2)

# install.packages('png')
library('png')

img.1 <- readPNG("./images/news.png")
img.2 <- readPNG("./images/user.png")

V(net2)$raster <- list(img.1, img.2)[V(net2)$type+1]

plot(net2, vertex.shape="raster", vertex.label=NA,
     vertex.size=16, vertex.size2=16, edge.width=2)

plot(net2, vertex.shape="raster", vertex.label=NA,
     vertex.size=16, vertex.size2=16, edge.width=2)

img.3 <- readPNG("./images/puppy.png")
rasterImage(img.3,  xleft=-1.6, xright=-0.6, ybottom=-1.1, ytop=0.1)

par(mfrow=c(1,2))

net2.bp <- bipartite.projection(net2) 

plot(net2.bp$proj1, vertex.label.color="black", vertex.label.dist=1,
     vertex.label=nodes2$media[!is.na(nodes2$media.type)])

plot(net2.bp$proj2, vertex.label.color="black", vertex.label.dist=1,
     vertex.label=nodes2$media[ is.na(nodes2$media.type)]) 

dev.off()


E(net)$width <- 1.5
plot(net, edge.color=c("dark red", "slategrey")[(E(net)$type=="hyperlink")+1],
     vertex.color="gray40", layout=layout_in_circle, edge.curved=.3)

net.m <- net - E(net)[E(net)$type=="hyperlink"] # another way to delete edges:
net.h <- net - E(net)[E(net)$type=="mention"]   # using the minus operator

# Plot the two links separately:
par(mfrow=c(1,2))
plot(net.h, vertex.color="orange", layout=layout_with_fr, main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", layout=layout_with_fr, main="Tie: Mention")


# Make sure the nodes stay in place in both plots:
l <- layout_with_fr(net)
plot(net.h, vertex.color="orange", layout=l, main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", layout=l, main="Tie: Mention")

dev.off()
multigtr <- graph( edges=c(1,2, 1,2, 1,2), n=2 )
l <- layout_with_kk(multigtr)

# Let's just plot the graph:
plot(multigtr, vertex.color="lightsteelblue", vertex.frame.color="white",
     vertex.size=40, vertex.shape="circle", vertex.label=NA,
     edge.color=c("gold", "tomato", "yellowgreen"), edge.width=5,
     edge.arrow.size=3, edge.curved=0.1, layout=l)

plot(multigtr, vertex.color="lightsteelblue", vertex.frame.color="white", 
     vertex.size=40, vertex.shape="circle", vertex.label=NA,
     edge.color=c("gold", "tomato", "yellowgreen"), edge.width=5,
     edge.arrow.size=3, edge.curved=curve_multiple(multigtr), layout=l)


detach('package:igraph')
#It is a good practice to detach packages 
#when we stop needing them. Try to remember that especially with igraph and the statnet family packages, as bad things tend to happen if you have them loaded together.

library('network')

net3 <- network(links,  vertex.attr=nodes, matrix.type="edgelist", 
                loops=F, multiple=F, ignore.eval = F)
net3[,]
net3 %n% "net.name" <- "Media Network" #  network attribute
net3 %v% "media"    # Node attribute
net3 %e% "type"     # Node attribute

net3 %v% "col" <- c("gray70", "tomato", "gold")[net3 %v% "media.type"]
plot(net3, vertex.cex=(net3 %v% "audience.size")/7, vertex.col="col")

l <- plot(net3, vertex.cex=(net3 %v% "audience.size")/7, vertex.col="col")
plot(net3, vertex.cex=(net3 %v% "audience.size")/7, vertex.col="col", coord=l)

detach('package:network')

plot(net3, vertex.cex=(net3 %v% "audience.size")/7, vertex.col="col", interactive=T)

library(ggraph)
library(igraph)

ggraph(net) +
  geom_edge_link() +   # add edges to the plot
  geom_node_point()    # add nodes to the plot


ggraph(net, layout="lgl") +
  geom_edge_link() +
  ggtitle("Look ma, no nodes!")  # add title to the plot

ggraph(net, layout="lgl") +
  geom_edge_fan(color="gray50", width=0.8, alpha=0.5) + 
  geom_node_point(color=V(net)$color, size=8) +
  theme_void()

ggraph(net, layout = 'linear') + 
  geom_edge_arc(color = "orange", width=0.7) +
  geom_node_point(size=5, color="gray80") +
  theme_void()

ggraph(net, layout="lgl") +
  geom_edge_link(aes(color = type)) +           # colors by edge type 
  geom_node_point(aes(size = audience.size)) +  # size by audience size  
  theme_void()

ggraph(net,  layout = 'lgl') +
  geom_edge_arc(color="gray", curvature=0.3) +            
  geom_node_point(color="orange", aes(size = audience.size)) +     
  geom_node_text(aes(label = media), size=3, color="gray50", repel=T) +
  theme_void()

detach("package:ggraph")

netm <- get.adjacency(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )

# Plot the egree distribution for our network:
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(degree(net)), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

# Animation NOT DONE YET

library('animation') 
library('igraph')

ani.options("convert") # Check that the package knows where to find ImageMagick
# If it doesn't know where to find it, give it the correct path for your system.
ani.options(convert="C:/Program Files/ImageMagick-6.8.8-Q16/convert.exe")
l <- layout_with_lgl(net)

saveGIF( {  col <- rep("grey40", vcount(net))
plot(net, vertex.color=col, layout=l)

step.1 <- V(net)[media=="Wall Street Journal"]
col[step.1] <- "#ff5100"
plot(net, vertex.color=col, layout=l)

step.2 <- unlist(neighborhood(net, 1, step.1, mode="out"))
col[setdiff(step.2, step.1)] <- "#ff9d00"
plot(net, vertex.color=col, layout=l) 

step.3 <- unlist(neighborhood(net, 2, step.1, mode="out"))
col[setdiff(step.3, step.2)] <- "#FFDD1F"
plot(net, vertex.color=col, layout=l)  },
interval = .8, movie.name="network_animation.gif" )

detach('package:igraph')
detach('package:animation')

install.packages("visNetwork")
library('visNetwork') 
visNetwork(nodes, links, width="100%", height="400px")

visNetwork(nodes, links, width="100%", height="400px", background="#eeefff",
           main="Network", submain="And what a great network it is!",
           footer= "Hyperlinks and mentions among media sources")

?visNodes
?visEdges

# We'll start by adding new node and edge attributes to our dataframes. 
vis.nodes <- nodes
vis.links <- links

vis.nodes$shape  <- "dot"  
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$media # Text on click
vis.nodes$label  <- vis.nodes$type.label # Node label
vis.nodes$size   <- vis.nodes$audience.size # Node size
vis.nodes$borderWidth <- 2 # Node border width

vis.nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"

visNetwork(vis.nodes, vis.links)


vis.links$width <- 1+links$weight/8 # line width
vis.links$color <- "gray"    # line color  
vis.links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- FALSE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow

visnet <- visNetwork(vis.nodes, vis.links)
visnet

visnet2 <- visNetwork(nodes, links)
visnet2 <- visNodes(visnet2, shape = "square", shadow = TRUE, 
                    color=list(background="gray", highlight="orange", border="black"))
visnet2 <- visEdges(visnet2, color=list(color="black", highlight = "orange"),
                    smooth = FALSE, width=2, dashes= TRUE, arrows = 'middle' ) 
visnet2

visOptions(visnet, highlightNearest = TRUE, selectedBy = "type.label")

nodes$group <- nodes$type.label 
visnet3 <- visNetwork(nodes, links)
visnet3 <- visGroups(visnet3, groupname = "Newspaper", shape = "square",
                     color = list(background = "gray", border="black"))
visnet3 <- visGroups(visnet3, groupname = "TV", shape = "dot",       
                     color = list(background = "tomato", border="black"))
visnet3 <- visGroups(visnet3, groupname = "Online", shape = "diamond",   
                     color = list(background = "orange", border="black"))
visLegend(visnet3, main="Legend", position="right", ncol=1) 

?visOptions # available options 
?visLayout  # available layouts
?visGroups  # using node groups
?visLegend  # adding a legend

# Detach the package since we're done with it. 
detach('package:visNetwork')

devtools::install_github('ramnathv/htmlwidgets')

library(threejs)
library(htmlwidgets)
library(igraph)

net.js <- net
graph_attr(net.js, "layout") <- NULL 

gjs <- graphjs(net.js, main="Network!", bg="gray10", showLabels=F, stroke=F, 
               curvature=0.1, attraction=0.9, repulsion=0.8, opacity=0.9)
print(gjs)
saveWidget(gjs, file="Media-Network-gjs.html")
browseURL("Media-Network-gjs.html")

gjs.an <- graphjs(net.js, bg="gray10", showLabels=F, stroke=F, 
                  layout=list(layout_randomly(net.js, dim=3),
                              layout_with_fr(net.js,  dim=3),
                              layout_with_drl(net.js, dim=3),  
                              layout_on_sphere(net.js)),
                  vertex.color=list(V(net.js)$color, "gray", "orange", 
                                    V(net.js)$color),
                  main=list("Random Layout", "Fruchterman-Reingold", 
                            "DrL layout", "Sphere" ) )
print(gjs.an)
saveWidget(gjs.an, file="Media-Network-gjs-an.html")
browseURL("Media-Network-gjs-an.html")

#install.packages("networkD3")
library(networkD3)

links.d3 <- data.frame(from=as.numeric(factor(links$from))-1, 
                       to=as.numeric(factor(links$to))-1 )

nodes.d3 <- cbind(idn=factor(nodes$media, levels=nodes$media), nodes) 

forceNetwork(Links = links.d3, Nodes = nodes.d3, Source="from", Target="to",
             NodeID = "idn", Group = "type.label",linkWidth = 1,
             linkColour = "#afafaf", fontSize=12, zoom=T, legend=T,
             Nodesize=6, opacity = 0.8, charge=-300, 
             width = 600, height = 400)

install.packages('ndtv', dependencies=T)
library('ndtv')
net3

par(mar=c(0,0,0,0))

render.d3movie(net3, usearrows = F, displaylabels = F, bg="#111111", 
               vertex.border="#ffffff", vertex.col =  net3 %v% "col",
               vertex.cex = (net3 %v% "audience.size")/8, 
               edge.lwd = (net3 %e% "weight")/3, edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net3 %v% 'media') , "<br>",
                                      "<b>Type:</b>", (net3 %v% 'type.label')),
               edge.tooltip = paste("<b>Edge type:</b>", (net3 %e% 'type'), "<br>", 
                                    "<b>Edge weight:</b>", (net3 %e% "weight" ) ),
               launchBrowser=F, filename="Media-Network.html" )  


data(short.stergm.sim)
short.stergm.sim 
head(as.data.frame(short.stergm.sim))

plot(short.stergm.sim)  

plot( network.extract(short.stergm.sim, at=1) )

plot( network.extract(short.stergm.sim, onset=1, terminus=5, rule="all") )

plot( network.extract(short.stergm.sim, onset=1, terminus=10, rule="any") ) 

render.d3movie(short.stergm.sim,displaylabels=TRUE) 

vs <- data.frame(onset=0, terminus=50, vertex.id=1:17)
es <- data.frame(onset=1:49, terminus=50, 
                 head=as.matrix(net3, matrix.type="edgelist")[,1],
                 tail=as.matrix(net3, matrix.type="edgelist")[,2])

net3.dyn <- networkDynamic(base.net=net3, edge.spells=es, vertex.spells=vs)

plot(net3.dyn, vertex.cex=(net3 %v% "audience.size")/7, vertex.col="col")

filmstrip(net3.dyn, displaylabels=F, mfrow=c(1, 5),
          slice.par=list(start=0, end=49, interval=10, 
                         aggregate.dur=10, rule='any'))

compute.animation(net3.dyn, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=50, interval=1, 
                                 aggregate.dur=1, rule='any'))

render.d3movie(net3.dyn, usearrows = F, 
               displaylabels = F, label=net3 %v% "media",
               bg="#ffffff", vertex.border="#333333",
               vertex.cex = degree(net3)/2,  
               vertex.col = net3.dyn %v% "col",
               edge.lwd = (net3.dyn %e% "weight")/3, 
               edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net3.dyn %v% "media") , "<br>",
                                      "<b>Type:</b>", (net3.dyn %v% "type.label")),
               edge.tooltip = paste("<b>Edge type:</b>", (net3.dyn %e% "type"), "<br>", 
                                    "<b>Edge weight:</b>", (net3.dyn %e% "weight" ) ),
               launchBrowser=T, filename="Media-Network-Dynamic.html",
               render.par=list(tween.frames = 30, show.time = F),
               plot.par=list(mar=c(0,0,0,0)), output.mode='inline' )


render.d3movie(net3.dyn, usearrows = F, 
               displaylabels = F, label=net3 %v% "media",
               bg="#000000", vertex.border="#dddddd",
               vertex.cex = function(slice){ degree(slice)/2.5 },  
               vertex.col = net3.dyn %v% "col",
               edge.lwd = (net3.dyn %e% "weight")/3, 
               edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net3.dyn %v% "media") , "<br>",
                                      "<b>Type:</b>", (net3.dyn %v% "type.label")),
               edge.tooltip = paste("<b>Edge type:</b>", (net3.dyn %e% "type"), "<br>", 
                                    "<b>Edge weight:</b>", (net3.dyn %e% "weight" ) ),
               launchBrowser=T, filename="Media-Network-even-more-Dynamic.html",
               render.par=list(tween.frames = 15, show.time = F), output.mode='inline',
               slice.par=list(start=0, end=50, interval=4, aggregate.dur=4, rule='any'))







