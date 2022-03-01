library("statnet")
library("igraph")
library("intergraph")
library("blockmodeling")
library("intergraph")
library("statnet")
library("tidyverse")
library("tidygraph")
library("ggraph")
library("ndtv")

# set up, read data 
data <- readxl::read_excel("final/data/QSS 41 Final Project Data.xlsx")
data

trimmed <- data %>% select(!`Academic Year (Start)`) %>% 
  mutate(`Class (Number)` = as.character(`Class (Number)`)) 

# make weighted for ggraph
trimmed_ggraph <- trimmed %>% 
  group_by(`Class (Number)`, `Professor (Last Name)`) %>% 
  summarize(n()) %>% 
  mutate(`Number of Courses` = `n()`)

trimmed

temp <- graph_from_data_frame(trimmed_ggraph)
V(temp)$type <- bipartite.mapping(temp)$type
V(temp)$type

edgelist_matrix <- as.matrix(trimmed)
network <- graph_from_edgelist(edgelist_matrix)

# basic info 
vcount(network)
ecount(network)
edge_density(network)
mean(degree(network))

# plot, both in base r and with ggraph 
ggraph(temp, layout = "fr") +
  geom_edge_link(aes(width = `Number of Courses`)) +
  geom_node_point(aes(color = V(temp)$type), size = 5) +
  geom_node_text(aes(label = name), size = 2) +
  scale_edge_width(range = c(0.5, 3)) +
  labs(color = "Professor?") +
  theme_graph()

# plot ggraph without legend 
ggraph(temp, layout = "fr") +
  geom_edge_link(aes(width = `Number of Courses`)) +
  geom_node_point(aes(color = V(temp)$type), size = 5) +
  geom_node_text(aes(label = name), size = 2) +
  scale_edge_width(range = c(0.5, 3)) +
  theme_graph() +
  theme(legend.position = "none")
  
V(network)$type <- bipartite.mapping(network)$type
plot(network, vertex.color = V(network)$type, vertex.size = 3, vertex.label.cex = 0.5,
     edge.arrow.size = 0, edge.width = E(network)$weight)

# plot weighted base r
weighted <- graph.adjacency(get.adjacency(network), weighted = TRUE)
V(weighted)$type <- bipartite.mapping(weighted)$type
plot(weighted, vertex.color = V(weighted)$type, vertex.size = 3, vertex.label.cex = 0.5,
     edge.arrow.size = 0, edge.width = E(weighted)$weight)

# get projection of matrix 
bipartite_matrix <- as_incidence_matrix(weighted)
proj_profs <- bipartite_matrix %*% t(bipartite_matrix)
diag(proj_profs) <- 0
prof_network <- graph_from_adjacency_matrix(proj_profs, mode = "undirected", weighted = TRUE)
plot(prof_network, vertex.size = 3, vertex.label.cex = 0.5,
     edge.arrow.size = 0, edge.width = E(prof_network)$weight)

ggraph(prof_network, layout = "fr") +
  geom_edge_link(aes(width = E(prof_network)$weight), color = "lightgray") +
  geom_node_point(size = 5, color = "coral2") +
  geom_node_text(aes(label = name), size = 3) +
  theme_graph() +
  theme(legend.position ="none")

ggraph(prof_network, layout = "fr") +
  geom_edge_link(aes(width = E(prof_network)$weight), color = "black") +
  geom_node_point(size = 5, color = "coral2") +
  geom_node_text(aes(label = name), size = 3, color = "blue") +
  theme_graph() +
  theme(legend.position ="none")

 
edge_density(prof_network)
vcount(prof_network)
mean(degree(prof_network))


# centrality calculations 

get_max <- function(vector) {
  max = 0
  name = 0
  for (i in 1:length(vector)) {
    if (vector[[i]] > max) {
      max = vector[[i]]
      name = names(vector[i])
    }
  }
  return(list(max, name))
}

betweenness(prof_network)
closeness(prof_network)

get_max(betweenness(prof_network))
get_max(eigen_centrality(prof_network)$vector)
get_max(degree(prof_network))
get_max(closeness(prof_network))


# clustering 

ebc <- edge.betweenness.community(prof_network, directed=F)

plot(as.dendrogram(ebc))

plot(ebc$modularity)

mods <- sapply(0:ecount(prof_network), function(i){
  g2 <- igraph::delete.edges(prof_network, ebc$removed.edges[seq(length=i)])
  cl <- clusters(g2)$membership
  modularity(prof_network,cl)
})
g2 <- igraph::delete.edges(prof_network, ebc$removed.edges[seq(which.max(mods)-1)])
plot(g2, vertex.size = 12)


ggraph(g2, layout = "fr") +
  geom_edge_link(color = "lightgray") +
  geom_node_point(size = 5, color = "coral2") +
  geom_node_text(aes(label = name), size = 3) +
  theme_graph() +
  theme(legend.position ="none")


clustered <- prof_network
ClusterMembership <- clusters(g2)$membership
V(clustered)$color <-  ClusterMembership
plot(clustered, vertex.label.cex = 0.5, vertex.size = 8)

membership(ebc)

fgc <- fastgreedy.community(prof_network)
plot(as.dendrogram(fgc))
plot(fgc$modularity)

fgc_mem <- membership(fgc)
fgc_temp <- tibble(prof1 = character(), prof2 = character()) 

isolates <- list()

for (i in 1:(length(fgc_mem) - 1)) {
  prof <- names(fgc_mem[i])
  isolate = TRUE
  for (j in (i + 1):length(fgc_mem)) {
    if (fgc_mem[[i]] == fgc_mem[[j]]) {
      isolate = FALSE
      if (get.edge.ids(prof_network, c(names(fgc_mem[i]), names(fgc_mem[j]))) != 0) {
        fgc_temp <- add_row(fgc_temp, prof1 = prof, prof2 = names(fgc_mem[j]))
      }
    }
  }
  if (isolate) {
    isolates <- append(isolates, names(fgc_mem[i]))
  }
}

fgc_temp

edgelist_matrix <- as.matrix(fgc_temp)
clustered_profs <- graph_from_edgelist(edgelist_matrix) %>% 
  add_vertices(length(isolates), name = isolates)
plot(clustered_profs, vertex.size = 10, edge.arrow.size = 0)

ggraph(clustered_profs, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "orange", size = 6) +
  geom_node_text(aes(label = name), size = 2, repel = TRUE) +
  theme_graph()


# blockmodeling 
adj_matrix <- as_adjacency_matrix(prof_network)
four_blocks <- optRandomParC(M = adj_matrix, k = 4, rep = 50, approaches = "hom",
                           homFun = "ss", blocks = "com")
three_blocks <- optRandomParC(M = adj_matrix, k = 3, rep = 50, approaches = "hom",
                             homFun = "ss", blocks = "com")
plot(four_blocks)
plot(three_blocks)
block_vector <- four_blocks$best$best1$clu
prof_network_blockmodel <- prof_network
V(prof_network_blockmodel)$color = block_vector
plot(prof_network_blockmodel, vertex.label.cex = 0.5, vertex.size = 8)

mixingmatrix(intergraph::asNetwork(prof_network_blockmodel), "color")

# dynamic network analysis 

# eighteen <- data %>% 
#   filter(`Academic Year (Start)` == 2018) %>% 
#   mutate(`Class (Number)` = as.character(`Class (Number)`)) %>% 
#   group_by(`Class (Number)`, `Professor (Last Name)`) %>% 
#   summarize(n()) %>% 
#   mutate(`Number of Courses` = `n()`)
# 
# graph_eighteen <- graph_from_data_frame(eighteen)
# V(graph_eighteen)$type <- bipartite.mapping(graph_eighteen)$type
# 
# ggraph(graph_eighteen, layout = "fr") +
#   geom_edge_link(aes(width = `Number of Courses`)) +
#   geom_node_point(aes(color = V(graph_eighteen)$type), size = 5) +
#   geom_node_text(aes(label = name), size = 2) +
#   scale_edge_width(range = c(0.5, 3)) +
#   labs(color = "Professor?") +
#   theme_graph()
# 
# 
# 
# ggraph(layout = "fr") +
#   geom_edge_link(data = eighteen, aes(width = `Number of Courses`)) +
#   geom_node_point(data = trimmed_ggraph, aes(color = V(temp)$type), size = 5) +
#   geom_node_text(data = trimmed_ggraph, aes(label = name), size = 2) +
#   scale_edge_width(range = c(0.5, 3)) +
#   labs(color = "Professor?") +
#   theme_graph()
# 
# 
# 
# time <- data %>% 
#   mutate(`Class (Number)` = as.character(`Class (Number)`)) 
# 
# base <- as_adjacency_matrix(network)
# base
# 
# eighteen <- time %>% 
#   filter(`Academic Year (Start)` == 2018)
# eighteen_df <- as.data.frame(eighteen)
# eighteen[,2]
# adj_eighteen <- base
# list_eighteen <- as.list(eighteen[,1])$`Professor (Last Name)`
# list_eighteen_2 <- as.list(eighteen[,2])$``
# list_eighteen <- list()
# for (i in 1:nrow(eighteen)) {
#   sliced <- eighteen %>% 
#     slice(i) %>% 
#     as.character()
#   list_eighteen[[i]] <- list(sliced[1], sliced[2])
# }
# list_eighteen
# prof_names <- dimnames(base)[[1]]
# class_names <- dimnames(base)[[2]]
# list(prof_names[1], class_names[2])
# 
# prof_names
# 
# for (i in 1:nrow(base)) {
#   for (j in 1:ncol(base)) {
#     print(prof_names[i])
#     print(class_names[j])
#     temp_list <- list(prof_names[i], class_names[j])
#     if (temp_list %in% list_eighteen) {
#       print("yes")
#     }
#   #   if (prof_names[i] %in% list_eighteen && class_names[i] %in% list_eighteen) {
#   #     adj_eighteen[i, j] <- 1
#   #   }
#   #   else {
#   #     adj_eighteen[i, j] <- 0
#   #   }
#   }
# }
# 
# nineteen <- time %>% 
#   filter(`Academic Year (Start)` == 2019)
# 
# adj_nineteen <- base
# list_nineteen <- as.list(nineteen[,1])$`Professor (Last Name)`
# 
# for (i in 1:nrow(base)) {
#   for (j in 1:ncol(base)) {
#     if (prof_names[i] %in% list_nineteen) {
#       adj_nineteen[i, j] <- 1
#     }
#     else {
#       adj_nineteen[i, j] <- 0
#     }
#   }
# }
# 
# twenty <- time %>% 
#   filter(`Academic Year (Start)` == 2020)
# 
# adj_twenty <- base
# list_twenty <- as.list(twenty[,1])$`Professor (Last Name)`
# 
# for (i in 1:nrow(base)) {
#   for (j in 1:ncol(base)) {
#     if (prof_names[i] %in% list_twenty) {
#       adj_twenty[i, j] <- 1
#     }
#     else {
#       adj_twenty[i, j] <- 0
#     }
#   }
# }
# 
# 
# twentyone <- time %>% 
#   filter(`Academic Year (Start)` == 2020)
# 
# adj_twentyone <- base
# list_twentyone <- as.list(twentyone[,1])$`Professor (Last Name)`
# 
# for (i in 1:nrow(base)) {
#   for (j in 1:ncol(base)) {
#     if (prof_names[i] %in% list_twentyone) {
#       adj_twentyone[i, j] <- 1
#     }
#     else {
#       adj_twentyone[i, j] <- 0
#     }
#   }
# }
# 
# animation <- networkDynamic(network.list = list(as.network(adj_eighteen),
#                                    as.network(adj_nineteen),
#                                    as.network(adj_twenty),
#                                    as.network(adj_twentyone)))
# render.d3movie(animation)




