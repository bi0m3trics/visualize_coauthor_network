if(!require("devtools")) install.packages("devtools")
library("devtools")
install.packages(c("R.cache","rvest"))
install_github("pablobarbera/scholarnetwork",dependencies = TRUE)
###########################################################################
library(XML)
getCompleteAuthors = function(id, pubid)
{
  auths = ""
  url_template = "http://scholar.google.com/citations?view_op=view_citation&citation_for_view=%s:%s"
  url = sprintf(url_template, id, pubid)
  #print("parsing html")
  tree = XML::htmlTreeParse(url, useInternalNodes = T,isURL=T)
  #print("finding authors")
  auths = xpathApply(tree, "//div[@class='gsc_value']",xmlValue)[[1]]
  return(auths)
}
###########################################################################
library(scholarnetwork)
extractNetwork <- function(id, n=500, largest_component=FALSE, ...){
  
  # downloading publications
  pubs <- scholar::get_publications(id=id, pagesize=n, ...)
  
  # converting to edges
  edges <- lapply(pubs$author, extractAuthors)
  edges <- do.call(rbind, edges)
  edges <- aggregate(edges$weight,
                     by=list(node1=edges$node1, node2=edges$node2),
                     FUN=function(x) sum(x))
  names(edges)[3] <- "weight"
  
  # extracting node-level information
  network <- igraph::graph.edgelist(as.matrix(edges[,c("node1", "node2")]), 
                                    directed=FALSE)
  igraph::edge_attr(network, "weight") <- edges$weight
  
  ### SELECT LARGEST COMPONENT
  if (largest_component==TRUE){
    network <- decompose(network)[[1]]
  }
  
  fc <- igraph::walktrap.community(network)
  nodes <- data.frame(label = igraph::V(network)$name,
                      degree=igraph::strength(network), group=fc$membership,
                      stringsAsFactors=F)
  nodes <- nodes[order(nodes$label),]
  if (largest_component==TRUE){
    edges <- edges[edges$node1 %in% nodes$label & edges$node2 %in% nodes$label,]
  }
  return(list(nodes=nodes, edges=edges))
}

extractAuthors <- function(x){
  authors <- unlist(stringr::str_split(x, ","))
  # deleting empty authors
  authors <- authors[grepl('[A-Za-z]+', authors)]
  # cleaning author list
  authors <- stringr::str_trim(authors)
  # keeping only initial of first name
  first <- gsub('(^[A-Z]{1}).*', authors, repl="\\1")
  last <- gsub("^[A-Z]* ([[:alnum:]'']+).*", authors, repl="\\1")
  #fix curly apostrophes
  last <- gsub('\'', "\\'", last)
  # fixing capitalization of last name
  last <- gsub("(^|'|'|[[:space:]])([[:alpha:]])", "\\1\\U\\2", last, perl=TRUE)
  last <- stringr::str_to_title(last)
  authors <- paste(first, last, sep=" ")
  # if more than one author, create edge list
  if (length(authors)>1){
    edges <- as.data.frame(t(combn(x=authors, m=2)), stringsAsFactors=F)
    names(edges) <- c("node1", "node2")
    edges$weight <- 1/length(authors)
    return(edges)
  }
  if (length(authors)<=1) return(NULL)
}

d <- extractNetwork(id="EwXUDAQAAAAJ", n=100000, largest_component = FALSE, flush=TRUE) # Me

# fix peoples' damn name!!?!??!
d$nodes$label[d$nodes$label=="A. J. S??Nchez Meador"]="A S?nchez Meador"
d$edges$node1[d$edges$node1=="A. J. S??Nchez Meador"]="A S?nchez Meador"
d$edges$node2[d$edges$node2=="A. J. S??Nchez Meador"]="A S?nchez Meador"

d$nodes$label[d$nodes$label=="Andrew J. S??Nchez Meador"]="A S?nchez Meador"
d$edges$node1[d$edges$node1=="Andrew J. S??Nchez Meador"]="A S?nchez Meador"
d$edges$node2[d$edges$node2=="Andrew J. S??Nchez Meador"]="A S?nchez Meador"

d$nodes$label[d$nodes$label=="Andrew S??Nchez Meador"]="A S?nchez Meador"
d$edges$node1[d$edges$node1=="Andrew S??Nchez Meador"]="A S?nchez Meador"
d$edges$node2[d$edges$node2=="Andrew S??Nchez Meador"]="A S?nchez Meador"

d$nodes$label[d$nodes$label=="A.j. S??Nchez Meador"]="A S?nchez Meador"
d$edges$node1[d$edges$node1=="A.j. S??Nchez Meador"]="A S?nchez Meador"
d$edges$node2[d$edges$node2=="A.j. S??Nchez Meador"]="A S?nchez Meador"

d$nodes$label[d$nodes$label=="B.j. De Blois"]="B De Blois"
d$edges$node1[d$edges$node1=="B.j. De Blois"]="B De Blois"
d$edges$node2[d$edges$node2=="B.j. De Blois"]="B De Blois"

# fix peoples' damn name!!?!??!
d$nodes$label[d$nodes$label=="P Fule"]="P Ful?"
d$edges$node1[d$edges$node1=="P Fule"]="P Ful?"
d$edges$node2[d$edges$node2=="P Fule"]="P Ful?"

#install.packages("extrafont")
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
l <- layout.fruchterman.reingold(network, niter=15000) # layout
fc <- walktrap.community(network, steps = 1) # community detection

# node locations
nodes <- data.frame(l); names(nodes) <- c("x", "y")
nodes$cluster <- factor(fc$membership)
nodes$label <- fc$names
nodes$degree <- degree(network)

# edge locations
edgelist <- get.edgelist(network, names=FALSE)
edges <- data.frame(nodes[edgelist[,1],c("x", "y")], nodes[edgelist[,2],c("x", "y")])
names(edges) <- c("x1", "y1", "x2", "y2")

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
network <- graph_from_data_frame(d$edges, directed=FALSE)
set.seed(5412)
l <- layout.fruchterman.reingold(network, niter=15000) # layout
fc <- walktrap.community(network, steps=1) # community detection
layout <-layout.fruchterman.reingold(network)
plot(fc, network, layout=layout, vertex.size=5,  edge.arrow.size=.2, edge.width=E(network)$weight*5, edge.curved=FALSE)

