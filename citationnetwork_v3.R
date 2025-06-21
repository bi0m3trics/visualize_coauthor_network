# =========================
# LOAD AND INSTALL PACKAGES
# =========================

if (!require("scholar")) install.packages("scholar")
if (!require("devtools")) install.packages("devtools")
if (!require("igraph")) install.packages("igraph")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("visNetwork")) install.packages("visNetwork")
if (!require("tidyr")) install.packages("tidyr")
if (!require("htmlwidgets")) install.packages("htmlwidgets")

library(scholar)
library(devtools)
if (!require("scholarnetwork")) install_github("pablobarbera/scholarnetwork")
library(scholarnetwork)
library(igraph)
library(dplyr)
library(stringr)
library(visNetwork)
library(tidyr)
library(htmlwidgets)

# =========================
# DATA RETRIEVAL AND CLEANING
# =========================

# Download publications from Google Scholar using your scholar ID
id <- "EwXUDAQAAAAJ"
user <- get_publications(id)

# Convert factor columns to character for easier manipulation
user[, sapply(user, class) == "factor"] <- as.character(user[, sapply(user, class) == "factor"])

# --------- AUTHOR NAME FIXES (MANUAL CORRECTIONS) ---------
# Fix common author name variants before further processing
fix_author_names <- function(authors) {
  # Ensure stringr is loaded
  require(stringr)
  
  # Standardize all variants to preferred forms
  authors <- str_replace_all(authors, "A\\.J\\.S Meador|AJ Sánchez Meador|A Sanchez-Meador|AJ Sanchez-Meador|AS Sánchez Meador|AJ Meador|AJ Sanchez Meador|AJ\\.S Meador|AJS Meador|AS Meador", "AJ Sánchez Meador")
  authors <- str_replace_all(authors, "PZ Ful(e|é)+", "PZ Fulé")
  authors <- str_replace_all(authors, "T Sankey|TTT Sankey|T\\. Sankey|T\\.T\\. Sankey", "TT Sankey")
  authors <- str_replace_all(authors, "\\bJ Sankey\\b", "JB Sankey")
  authors <- str_replace_all(authors, "\\bD Huffman\\b", "DW Huffman")
  authors <- str_replace_all(authors, "\\bT Kolb\\b", "TE Kolb")
  authors <- str_replace_all(authors, "\\bK Waring\\b", "KM Waring")
  authors <- str_replace_all(authors, "S Nepal+", "S Nepal")
  authors <- str_replace_all(authors, "\\bA Springer\\b", "AE Springer")
  authors <- str_replace_all(authors, "\\bK Rodman\\b", "KC Rodman")
  authors <- str_replace_all(authors, "FD Boissieu|F de Boissieu|F Boissieu|F de Boissieun", "F De Boissieu")
  authors <- str_replace_all(authors, "J-R Roussel", "JR Roussel")
  authors <- str_replace_all(authors, "J Donager|JJJ Donager", "JJ Donager")
  authors <- str_replace_all(authors, "\\bB Bagdon\\b", "BA Bagdon")
  authors <- str_replace_all(authors, "R\\. Buscaglia|R Buscaglia", "R Buscaglia")
  
  # Preferred forms for ambiguous initials (second is preferred)
  authors <- str_replace_all(authors, "\\bD Affleck\\b|\\bDLR Affleck\\b", "DLR Affleck")
  authors <- str_replace_all(authors, "\\bJ Cannon\\b|\\bJB Cannon\\b", "JB Cannon")
  authors <- str_replace_all(authors, "\\bM Colavito\\b|\\bMM Colavito\\b", "MM Colavito")
  authors <- str_replace_all(authors, "\\bJ Coulston\\b|\\bJW Coulston\\b", "JW Coulston")
  authors <- str_replace_all(authors, "\\bJJ Donager\\b|\\bJJJ Donager\\b", "JJ Donager")
  authors <- str_replace_all(authors, "\\bA Gray\\b|\\bAN Gray\\b", "AN Gray")
  authors <- str_replace_all(authors, "\\bD Laughlin\\b|\\bDC Laughlin\\b", "DC Laughlin")
  authors <- str_replace_all(authors, "\\bC Lorimer\\b|\\bCG Lorimer\\b", "CG Lorimer")
  authors <- str_replace_all(authors, "\\bD MacFarlane\\b|\\bDW MacFarlane\\b", "DW MacFarlane")
  authors <- str_replace_all(authors, "\\bK Poudel\\b|\\bKP Poudel\\b", "KP Poudel")
  authors <- str_replace_all(authors, "\\bP Radtke\\b|\\bPJ Radtke\\b", "PJ Radtke")
  authors <- str_replace_all(authors, "\\bJ Shaw\\b|\\bJD Shaw\\b", "JD Shaw")
  authors <- str_replace_all(authors, "\\bC Stevens-Rumann\\b|\\bCS Stevens-Rumann\\b", "CS Stevens-Rumann")
  authors <- str_replace_all(authors, "\\bM Tuten\\b|\\bMC Tuten\\b", "MC Tuten")
  authors <- str_replace_all(authors, "\\bAE Waltz\\b|\\bAEM Waltz\\b", "AEM Waltz")
  authors <- str_replace_all(authors, "\\bA Weiskittel\\b|\\bAR Weiskittel\\b", "AR Weiskittel")
  authors <- str_replace_all(authors, "\\bJ Westfall\\b|\\bJA Westfall\\b", "JA Westfall")
  authors
}
  
user$author <- sapply(user$author, fix_author_names)

# =========================
# HANDLE ELLIPSIS AUTHORS
# =========================

ellipsis_indices <- grep(user$author, pattern = "\\.\\.\\.")
author_complete <- sapply(user$pubid[ellipsis_indices], get_complete_authors, id = id)

lowerName <- function(x) {gsub(x, pattern = "\ ([A-Z]){1}([A-Z]+)", replacement = '\ \\1\\L\\2', perl = TRUE)}
initialName <- function(x) {gsub(x, pattern = "([A-Za-z]){1}([a-z]+)", replacement = '\\1', perl = TRUE)}

author_complete_reformat <- lapply(author_complete, function(elem) {
  split_elem <- strsplit(elem, ", ")[[1]]
  split_elem <- sapply(split_elem, gsub, pattern = "(\\x.{2})+", replacement = "")
  rename_elem <- sapply(split_elem, function(name) {
    name2 <- iconv(name, "latin1", "ASCII", sub = "")
    name2 <- lowerName(name2)
    name2 <- strsplit(name2, " ")[[1]]
    lastname <- dplyr::last(name2)
    if (length(name2) > 1) {
      name2 <- sapply(1:(length(name2)-1), function(i) {initialName(lowerName(name2[i]))})
      name2 <- paste(paste(name2, collapse = ""), lastname, sep = " ")
    } else {
      name2 <- lastname
    }
    return(name2)
  })
  rename_elem <- paste(rename_elem, collapse = ", ")
  return(rename_elem)
})

user$author_orig <- user$author
user$author <- as.character(user$author)
user$author[ellipsis_indices] <- author_complete_reformat

user.coauthors <- sapply(as.character(user$author), strsplit, ", ")
user.coauthors <- lapply(user.coauthors, trimws)

# =========================
# UNIVERSAL AUTHOR NAME STANDARDIZATION
# =========================

standardize_author <- function(name) {
  name <- gsub("\\.", "", name)
  name <- gsub("\\s+", " ", name)
  name <- str_trim(name)
  name <- stringi::stri_trans_general(name, "Latin-ASCII")
  name <- str_to_title(name)
  
  # Fix common truncations
  name <- gsub("Tarancn$", "Tarancon", name)
  name <- gsub("Ful$", "Fule", name)
  
  # Collapse multiple initials
  parts <- unlist(strsplit(name, " "))
  if (length(parts) > 1) {
    initials <- gsub("[^A-Z]", "", toupper(parts[-length(parts)]))
    # Use only the first two initials for grouping
    initials <- paste0(substr(paste(initials, collapse = ""), 1, 2))
    lastname <- parts[length(parts)]
    name <- paste(initials, lastname)
  }
  
  # Special case: any Sanchez or Meador
  if (grepl("sanchez|meador", name, ignore.case = TRUE)) {
    return("Andrew Sanchez Meador")
  }
  
  return(name)
}

user.coauthors <- lapply(
  user.coauthors,
  function(authors) sapply(authors, standardize_author)
)

all_names <- unique(unlist(user.coauthors))
get_key <- function(name) {
  parts <- unlist(strsplit(name, " "))
  if (length(parts) == 1) return(parts)
  last <- parts[length(parts)]
  initials <- paste(substr(parts[-length(parts)], 1, 1), collapse = "")
  key <- paste0(initials, " ", last)
  key
}
name_keys <- sapply(all_names, get_key)
canonical_names <- tapply(all_names, name_keys, function(x) x[which.max(nchar(x))])
name_to_canonical <- setNames(canonical_names[name_keys], all_names)
user.coauthors <- lapply(
  user.coauthors,
  function(authors) unname(name_to_canonical[authors])
)

# =========================
# BUILD EDGE AND NODE LISTS
# =========================

edge_list <- do.call(rbind, lapply(user.coauthors, function(authors) {
  if (length(authors) > 1) {
    t(combn(authors, 2))
  } else {
    NULL
  }
}))
edge_df <- as.data.frame(edge_list, stringsAsFactors = FALSE)
colnames(edge_df) <- c("from", "to")

edge_df <- edge_df %>%
  group_by(from, to) %>%
  summarise(value = n(), .groups = "drop")

andrew <- "Andrew Sanchez Meador"

andrew_coauthors <- unique(
  unlist(
    lapply(user.coauthors, function(authors) {
      if (andrew %in% authors) setdiff(authors, andrew) else NULL
    })
  )
)

get_unique_andrew_links <- function(name) {
  if (name == andrew) {
    length(andrew_coauthors)
  } else {
    sum(sapply(user.coauthors, function(authors) andrew %in% authors && name %in% authors))
  }
}

nodes <- data.frame(id = unique(c(edge_df$from, edge_df$to)), stringsAsFactors = FALSE)
nodes$unique_andrew_links <- sapply(nodes$id, get_unique_andrew_links)

max_other <- max(nodes$unique_andrew_links[nodes$id != andrew], na.rm = TRUE)
nodes$size <- nodes$unique_andrew_links + 5
nodes$size[nodes$id == andrew] <- max_other + 5

g <- igraph::graph_from_data_frame(edge_df, directed = FALSE, vertices = nodes)
cl <- igraph::cluster_walktrap(g)
membership <- igraph::membership(cl)
nodes$group <- as.factor(membership[nodes$id])
nodes$color.background <- ifelse(nodes$id == andrew, "grey", NA)
nodes$label <- nodes$id

edge_df$width <- apply(edge_df, 1, function(row) {
  if (row["from"] == andrew) {
    nodes$size[nodes$id == row["to"]]
  } else if (row["to"] == andrew) {
    nodes$size[nodes$id == row["from"]]
  } else {
    1
  }
})

# Alphabetize dropdown by last name
get_lastname <- function(x) tail(unlist(strsplit(x, " ")), 1)
nodes <- nodes[order(sapply(nodes$label, get_lastname)), ]

network_widget <- visNetwork(
  nodes, edge_df,
  width = "100vw", height = "100vh", background = "black"
) %>%
  visEdges(width = "width") %>%
  visNodes(
    size = "size",
    font = list(size = 32, color = "white"),
    color = list(background = nodes$color.background, border = "white", highlight = "white")
  ) %>%
  visOptions(
    highlightNearest = TRUE,
    nodesIdSelection = list(
      enabled = TRUE,
      style = "background: black; color: white; border: 1px solid white;",
      main = "Select by Coauthor",
      values = nodes$id
    )
  ) %>%
  visIgraphLayout(layout = "layout_with_fr")

saveWidget(
  network_widget,
  file = "andrew_coauthor_network.html",
  selfcontained = TRUE,
  title = "Andrew Sanchez Meador Coauthor Network",
  background = "black"
)
