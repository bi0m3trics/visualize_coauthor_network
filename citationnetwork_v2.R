# Based largely on what I learned suing these two scripts
# https://kateto.net/netscix2016.html
# https://datalab.ucdavis.edu/2019/08/27/creating-co-author-networks-in-r/

library(scholar)
library(scholarnetwork)
library(igraph)
library(dplyr)

id="EwXUDAQAAAAJ" # id from url on Google Scholar page
user = get_publications(id) 

# convert factor variables to strings
user[,sapply(user, class)=="factor"] = 
  as.character(user[,sapply(user, class)=="factor"])

ellipsis_indices = grep(user$author, pattern =  "\\.\\.\\.")
author_complete = sapply(user$pubid[ellipsis_indices], 
                         get_complete_authors, id = id)

# helper function to put last names in a regular format (first letter uppercase, rest lowercase):
lowerName <- function(x) {gsub(x, pattern = "\ ([A-Z]){1}([A-Z]+)", replacement = '\ \\1\\L\\2', perl = TRUE)}

# helper function to convert a name to an initial (applied after lowername):
initialName <- function(x) {gsub(x, pattern = "([A-Za-z]){1}([a-z]+)", replacement = '\\1', perl = TRUE)}

#  Reformat the author name so they are standardized across manuscripts
author_complete_reformat = lapply(author_complete, function(elem) {
  
  # Split strings of authors into individual authors based on commas:
  split_elem = strsplit(elem, ", ")[[1]]
  split_elem = sapply(split_elem, gsub, pattern = "(\\x.{2})+", replacement ="")
  
  # Put author names into INITIALS, Lastname format:
  rename_elem = sapply(split_elem, function(name) {
    
    #in case of name like "H J\xfc\xbe\x8d\x86\x94\xbcrvinen":
    name2 = iconv(name, "latin1", "ASCII", sub = "") 
    name2 = lowerName(name2)
    name2 = strsplit(name2, " ")[[1]]
    
    lastname = last(name2)
    
    if (length(name2) > 1) {
      name2 =  sapply(1:(length(name2)-1), function(i) {initialName(lowerName(name2[i]))})
      name2 = paste(paste(name2, collapse = ""), lastname, sep = " ")
    } else {
      name2 = lastname
    }
    return(name2)
  })
  
  # Put separated names back in single strings:
  rename_elem = paste(rename_elem, collapse = ", ")
  
  return(rename_elem)
})

# Save original author column as "author_orig" and update the "author column"
user$author_orig = user$author
user$author = as.character(user$author)
user$author[ellipsis_indices] = author_complete_reformat

# Clean up all the random name issues - you may not have to do this if your Google Scholar is clean
user$author = sapply(user$author, str_replace, pattern = "A.J.S Meador", replacement = "AJ Sánchez Meador")
user$author = sapply(user$author, str_replace, pattern = "AJ Meador", replacement = "AJ Sánchez Meador")
user$author = sapply(user$author, str_replace, pattern = "AJ SÃ¡nchez Meador", replacement = "AJ Sánchez Meador")
user$author = sapply(user$author, str_replace, pattern = "AJ Sanchez Meador", replacement = "AJ Sánchez Meador")
user$author = sapply(user$author, str_replace, pattern = "AJ.S Meador", replacement = "AJ Sánchez Meador")
user$author = sapply(user$author, str_replace, pattern = "AJS Meador", replacement = "AJ Sánchez Meador")
user$author = sapply(user$author, str_replace, pattern = "AS Meador", replacement = "AJ Sánchez Meador")
user$author = sapply(user$author, str_replace_all, pattern = "PZ Ful ", replacement = "PZ Fulé")
user$author = sapply(user$author, str_replace_all, pattern = "PZ Ful", replacement = "PZ Fulé")
user$author = sapply(user$author, str_replace, pattern = "PZ Fulée", replacement = "PZ Fulé")
user$author = sapply(user$author, str_replace, pattern = "PZ Fuléé", replacement = "PZ Fulé")
user$author = sapply(user$author, str_replace, pattern = "T Sankey", replacement = "TT Sankey")
user$author = sapply(user$author, str_replace, pattern = "TTT Sankey", replacement = "TT Sankey")
user$author = sapply(user$author, str_replace, pattern = "J Sankey", replacement = "JB Sankey")
user$author = sapply(user$author, str_replace, pattern = "AA Taracn", replacement = "A Azpeleta Tarancón")
user$author = sapply(user$author, str_replace, pattern = "AA Tarancn", replacement = "A Azpeleta Tarancón")
user$author = sapply(user$author, str_replace, pattern = "AA Tarancón", replacement = "A Azpeleta Tarancón")
user$author = sapply(user$author, str_replace, pattern = "A Azpeleta", replacement = "A Azpeleta Tarancón")
user$author = sapply(user$author, str_replace, pattern = "A Azpeleta Tarancón Tarancón", replacement = "A Azpeleta Tarancón")
user$author = sapply(user$author, str_replace, pattern = "D Huffman", replacement = "DW Huffman")
user$author = sapply(user$author, str_replace, pattern = "T Kolb", replacement = "TE Kolb")
user$author = sapply(user$author, str_replace, pattern = "K Waring", replacement = "KM Waring")
user$author = sapply(user$author, str_replace, pattern = "S Nepal­­", replacement = "S Nepal")
user$author = sapply(user$author, str_replace, pattern = "A Springer", replacement = "AE Springer")
user$author = sapply(user$author, str_replace, pattern = "K Rodman", replacement = "KC Rodman")
user$author = sapply(user$author, str_replace, pattern = "FD Boissieu", replacement = "F De Boissieu")
user$author = sapply(user$author, str_replace, pattern = "F de Boissieu", replacement = "F De Boissieu")
user$author = sapply(user$author, str_replace, pattern = "F Boissieu", replacement = "F De Boissieu")
user$author = sapply(user$author, str_replace, pattern = "F de Boissieun", replacement = "F De Boissieu")
user$author = sapply(user$author, str_replace, pattern = "J-R Roussel", replacement = "JR Roussel")
user$author = sapply(user$author, str_replace, pattern = "J Donager", replacement = "JJ Donager")
user$author = sapply(user$author, str_replace, pattern = "JJJ Donager", replacement = "JJ Donager")
user$author = sapply(user$author, str_replace, pattern = "B Bagdon", replacement = "BA Bagdon")

# Build the list of coauthors
user.coauthors = sapply(as.character(user$author), strsplit, ", ")

# Get rid of white space and superfluous periods
user.coauthors = lapply(user.coauthors, trimws)
user.coauthors = lapply(user.coauthors, function(x) str_replace_all(x,"\\.",""))

# Get the unique authors form the cleaned list
user.coauthors.unique = unique(unlist(user.coauthors))[order(unique(unlist(user.coauthors)))]

# Get the numbers of publications by coauthor
sort(table(unlist(user.coauthors)), decreasing = TRUE)

# Get the connections that involve or are made by two separate parties.
user.bipartite.edges = lapply(user.coauthors, function(x) {user.coauthors.unique %in% x})
user.bipartite.edges = do.call("cbind", user.bipartite.edges) # dimension is number of authors x number of papers
rownames(user.bipartite.edges) = user.coauthors.unique

user.mat = user.bipartite.edges %*% t(user.bipartite.edges) #bipartite to unimode
mat = user.mat[order(rownames(user.mat)), order(rownames(user.mat))]

################################################################################
# This visualizes it using the plot.network form the network pacjage, but I like 
# igraph better
# user.statnet = as.network(user.mat, directed = FALSE, names.eval = "edge.lwd",
# ignore.eval = FALSE)
# user.statnet # view network summary
# 
# plot.network(user.statnet, edge.col = "gray", edge.lwd = "edge.lwd", 
#              label = "vertex.names", label.cex = .5, label.pad = 0, 
#              label.pos = 1, mode="fruchtermanreingold") #kamadakawai or circle
################################################################################

# Now let's visualize it using igraph and set the fonts
library(extrafont)
loadfonts()

# Build the adjacency matrix from the matirx
network <- graph_from_adjacency_matrix(mat, mode = "undirected")

# find the densely connected clusters in the network
fc <- cluster_walktrap(network, steps = 4)

# Set the colors using ochRe
# devtools::install_github("ropenscilabs/ochRe")
library(ochRe)
pal<-colorRampPalette(ochre_palettes[["mccrea"]])
new_cols <- pal(max(fc$membership))[membership(fc)]
V(network)$color = new_cols

# Plot it...
plot(fc, network, vertex.size=5, edge.arrow.size=.2, 
     edge.width=E(network)$weight*2, vertex.label.family = "Calibri",
     edge.curved=FALSE,  axes = FALSE, mark.border="black", edge.color = "black",
     vertex.label.cex = 0.7, vertex.label.family = 2, mark.col=new_cols, vertex.label.color="black")
