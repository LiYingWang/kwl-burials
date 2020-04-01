library(network)

burial_network_pre <-
  network(edges_for_network_pre, # the network object
          vertex.attr = nodes_pre, # node list
          directed = FALSE, # specify whether the network is directed
          ignore.eval = FALSE, # FALSE = weighted
          loops = FALSE, # do we allow self ties (should not allow them)
          matrix.type = "edgelist") # the type of input

# plot
plot(burial_network_pre, vertex.cex = 1) # It seems the last two nodes are missing?? only 27 nodes


library(tidygraph)
library(ggraph)

relation_tidy_pre <- tbl_graph(nodes = nodes_pre,
                               edges = edges_for_network_pre,
                               directed = FALSE)

relation_tidy_pre %>%
  activate(edges) %>%
  arrange(desc(common_counts))

ggraph(relation_tidy_pre) +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

ggraph(relation_tidy_pre, layout = "graphopt") +
  geom_node_point() +
  geom_edge_link(alpha = 0.8) +
  scale_edge_width(range = c(0.2, 2)) +
  labs(edge_width = "common item") +
  theme_graph()


rstat_nodes <- data.frame(name = c("Hadley", "David", "Romain", "Julia", "Emily"))
rstat_edges <- data.frame(from = c(1, 1, 1, 2, 3, 3, 4, 4, 4),
                          to = c(2, 3, 4, 1, 1, 2, 1, 2, 3))
graph_output <- tbl_graph(nodes = rstat_nodes, edges = rstat_edges)
ggraph(graph_output, layout = "graphopt") +
  geom_node_point() +
  geom_edge_link(alpha = 0.8) +
  scale_edge_width(range = c(0.2, 2)) +
  labs(edge_width = "common item") +
  theme_graph()

#------network
rstat_nodes$id <- 1:nrow(rstat_nodes)
example_network <-
  network(rstat_edges, # the network object
          vertex.attr = tibble(rstat_nodes), # node list
          directed = FALSE, # specify whether the network is directed
          ignore.eval = FALSE, # FALSE = weighted
          loops = FALSE, # do we allow self ties (should not allow them)
          matrix.type = "edgelist") # the type of input

# plot
plot(example_network, vertex.cex = 1) # It seems the last two nodes are missing?? only 27 nodes
