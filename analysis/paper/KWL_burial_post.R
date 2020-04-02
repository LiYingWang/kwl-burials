#-----------------------European----------------------------
# filter post burials
burial_post <-
  burial_three_period_tidy %>%
  filter(Phase == "post") %>%
  janitor::remove_empty(which = "cols")

# create node list
nodes_post <-
  burial_three_period_tidy %>%
  filter(Phase == "post") %>%
  select(burial_label) %>%
  rowid_to_column("id")

# pair wise combinations for burials as index for later map function
library(gtools)
burial_comb_post = combinations(length(nodes_post$burial_label),
                               2, nodes_post$burial_label,
                               repeats = TRUE)
colnames(burial_comb_post) = c("burial_1", "burial_2")
burial_comb_post = as_tibble(burial_comb_post)

# create list for each burial that contains the burial good types and their counts
edge_list_post <-
  burial_three_period_tidy %>%
  select(burial_label, 3:10) %>% # need to change for each exploration
  pivot_longer(-burial_label, names_to = "goods", values_to = "count") %>%
  group_by(burial_label) %>%
  nest()

# pair-wise list
common_counts_lst_post <-
  map2(burial_comb_post$burial_1,
       burial_comb_post$burial_2,
       ~bind_cols(
         edge_list_post %>%
           filter(burial_label == .x) %>%
           unnest(data),
         edge_list_post %>%
           filter(burial_label == .y) %>%
           unnest(data)) %>%
         rowwise() %>%
         mutate(common_counts = sum(count, count1)))

# count of ornament types in common between each pair of burials
common_counts_vct_post <- map_int(common_counts_lst_post, ~sum(!is.na(.x$common_counts)))

burial_comb_with_common_counts_post <-
  burial_comb_post %>%
  mutate(common_counts = common_counts_vct_post) %>%
  mutate(common_counts = ifelse(burial_1 == burial_2, 0, common_counts)) # use 0 for self loop

# change label to ids for node linking
edges_post <-
  burial_comb_with_common_counts_post %>%
  left_join(nodes_post, by = c("burial_1" = "burial_label")) %>%
  rename(from = id) %>%
  left_join(nodes_post, by = c("burial_2" = "burial_label")) %>%
  rename(to = id)

edges_for_network_post <-
  select(edges_post, from, to, common_counts) %>%
  filter (!common_counts == 0) # remove rows with no goods in common

#1--------------------network analysis using ggraph pkg------------------------------
library(tidygraph)
library(ggraph)

relation_tidy_post <- tbl_graph(nodes = nodes_post,
                               edges = edges_for_network_post,
                               directed = FALSE)

relation_tidy_post %>%
  activate(edges) %>%
  arrange(desc(common_counts))

ggraph(relation_tidy_post) +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

ggraph(relation_tidy_post, layout = "graphopt") +
  geom_node_point() +
  geom_edge_link(aes(width = common_counts), alpha = 0.8) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = burial_label), repel = TRUE) +
  labs(edge_width = "common item") +
  theme_graph()

#2-------------------network analysis using network pkg-------------------------------
library(network)

burial_network_post <-
  network(edges_for_network_post, # the network object
          vertex.attr = nodes_post, # node list
          directed = FALSE, # specify whether the network is directed
          ignore.eval = FALSE, # FALSE = weighted
          loops = FALSE, # do we allow self ties (should not allow them)
          matrix.type = "edgelist") # the type of input

# plot
plot(burial_network_post, vertex.cex = 1) # It seems the last two nodes are missing?? only 27 nodes

#-----------------------Bayesian ERGMs------------------------------
library(statnet)
library(Bergm)

# Add attributes to the network object burial_network_pre
set.vertex.attribute(burial_network_post, "quantity", burial_post$quantity)

# plot
set.seed(30)
quantity <- get.vertex.attribute(burial_network_post, "quantity")
ID <- get.vertex.attribute(burial_network_post, "burial_label") # not sure how to get id on the network plot
plot(burial_network_post,
     vertex.col = "quantity",
     vertex.cex = 1.5)

legend("topleft",
       col = c(2, 3, 1, 4), # need to adjust each time
       pch    = 20,
       legend = unique(quantity),
       title  = 'Burial good counts')
