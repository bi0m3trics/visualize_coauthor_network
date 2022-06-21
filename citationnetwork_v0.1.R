if(!require("devtools")) install.packages("devtools")
library("devtools")
install_github("pablobarbera/scholarnetwork")

###########################################################################
library(scholarnetwork)
d <- extractNetwork(id="EwXUDAQAAAAJ", n=100000, largest_component = FALSE) # Me
plotNetwork(d$nodes, d$edges, width = 1024,
            height = 768, file="network.html", 
            fontsize = 10, charge = -500)

# fix peoples' damn name!!?!??!
d$nodes$label[d$nodes$label=="A S?nchez"]="A S?nchez Meador"
d$edges$node1[d$edges$node1=="A S?nchez"]="A S?nchez Meador"
d$edges$node2[d$edges$node2=="A S?nchez"]="A S?nchez Meador"

d$nodes$label[d$nodes$label=="A Sanchez"]="A S?nchez Meador"
d$edges$node1[d$edges$node1=="A Sanchez"]="A S?nchez Meador"
d$edges$node2[d$edges$node2=="A Sanchez"]="A S?nchez Meador"

d$nodes$label[d$nodes$label=="A Meador"]="A S?nchez Meador"
d$edges$node1[d$edges$node1=="A Meador"]="A S?nchez Meador"
d$edges$node2[d$edges$node2=="A Meador"]="A S?nchez Meador"

d$nodes$label[d$nodes$label=="B De"]="B De Blois"
d$edges$node1[d$edges$node1=="B De"]="B De Blois"
d$edges$node2[d$edges$node2=="B De"]="B De Blois"

d$nodes$label[d$nodes$label=="F O'donnell"]="F ODonnell"
d$edges$node1[d$edges$node1=="F O'donnell"]="F ODonnell"
d$edges$node2[d$edges$node2=="F O'donnell"]="F ODonnell"

d$nodes$label[d$nodes$label=="P Fule"]="P Ful?"
d$edges$node1[d$edges$node1=="P Fule"]="P Ful?"
d$edges$node2[d$edges$node2=="P Fule"]="P Ful?"

# install.packages("extrafont")
library(extrafont)
#font_import()
loadfonts()
#fonts()

library(ggplot2)
library(igraph)
library(plotrix)
# cleaning network data
network <- graph_from_data_frame(d$edges, directed=FALSE)
set.seed(5412)
l <- layout_with_fr(network, niter=15000, dim=3) # layout
fc <- walktrap.community(network, steps = 10) # community detection
plot(fc, network, vertex.size=5, edge.arrow.size=.2, edge.width=E(network)$weight*2, 
     edge.curved=FALSE,  axes = FALSE,
     vertex.label.cex = 0.5, vertex.label.family = "Calibri")

# https://kateto.net/netscix2016.html

# node locations
nodes <- data.frame(l); names(nodes) <- c("x", "y")
nodes$cluster <- factor(fc$membership)
nodes$label <- fc$names
nodes$degree <- degree(network)

# edge locations
edgelist <- get.edgelist(network, names=FALSE)
edges <- data.frame(nodes[edgelist[,1],c("x", "y")], nodes[edgelist[,2],c("x", "y")])
names(edges) <- c("x1", "y1", "x2", "y2")
labels<-d$nodes$label

# and now visualizing it...
ggplot(nodes, aes(x=x, y=y, color=cluster, label=label, size=degree)) +
  # edges
  geom_segment(
    aes(x=x1, y=y1, xend=x2, yend=y2, label=NA),
    data=edges, size=rescale(d$edges$weight, c(0.1,5)), color="grey20", alpha=1/5) +  
  # nodes
  geom_point(color="grey20", aes(fill=cluster),
             shape=21, show.legend=FALSE, alpha = 1/2) +
  # labels
  geom_text(color="black", aes(label=label, size=rescale(degree, c(0.001,3))),
                    show.legend=FALSE) +
  
  ## note that here I add a border to the points
  scale_fill_discrete(labels=labels) +
  scale_size_continuous(range = c(3, 8)) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill="white"),
    axis.line = element_blank(), axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(), panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(colour = F, fill = "black"),
    legend.key = element_rect(fill = "black", colour = F),
    legend.title = element_text(color="white"),
    legend.text = element_text(color="white")
  ) +
  ## changing size of points in legend
  guides(fill = guide_legend(override.aes = list(size=degree)))

#############################################################################################
# Another verison using only the igraph package... and plotting polygons around your clusters
#############################################################################################

# cleaning network data
network <- graph_from_data_frame(d$edges, directed=TRUE)
network
set.seed(1)
l <- layout.fruchterman.reingold(network, niter=250) # layout
fc <- walktrap.community(network, steps=4) # community detection
plot(fc, network, layout=layout_nicely(network), vertex.size=3,
     edge.arrow.size=.2, edge.width=E(network)$weight*5, edge.curved=FALSE,
     axes = FALSE, mark.groups = communities(fc), mark.shape = 1/2,
     mark.expand = 20)

