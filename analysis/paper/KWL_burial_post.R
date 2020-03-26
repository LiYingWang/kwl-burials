#-----------------------European----------------------------
nodes_post <- 
  burial_three_period_tidy %>% 
  filter(Phase == "post") %>% 
  select(burial_label) %>% 
  rowid_to_column("id")

# pair wise combinations for burials as index for later map fuction
burial_comb_post = t(combn(nodes_post$burial_label, 2))
colnames(burial_comb_post) = c("burial_1", "burial_2")
burial_comb_post = as_tibble(burial_comb_post)

edge_list_post <-
  burial_three_period_tidy %>% 
  select(burial_label, 19:38) %>% 
  pivot_longer(-burial_label, names_to = "goods", values_to = "count") %>% 
  #mutate(burial_connection = rep(unique(burial_label), length.out = length(burial_label)))
  group_by(burial_label) %>% 
  nest() 

# pair wise list 
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
  mutate(common_counts = common_counts_vct_post)

# change label to ids for node linking
edges_post <- 
  burial_comb_with_common_counts_post %>% 
  left_join(nodes_post, by = c("burial_1" = "burial_label")) %>% 
  rename(from = id) %>% 
  left_join(nodes_post, by = c("burial_2" = "burial_label")) %>% 
  rename(to = id) 

edges_for_network_post <- select(edges_post, from, to, common_counts)
# -------------------network analysis 1-------------------------------
library(network)
burial_network_post <- 
  network(edges_for_network_post, 
          vertex.attr = nodes_post, 
          matrix.type = "edgelist", 
          ignore.eval = FALSE,
          directed=FALSE
  )

# plot
plot(burial_network_post, vertex.cex = 3)

#--------------------network analysis 2------------------------------
detach(package:network)
rm(burial_network_post)

library(tidygraph)
library(ggraph)

relation_tidy_post <- tbl_graph(nodes = nodes_post, edges = edges_for_network_post, directed = FALSE)

relation_tidy_post %>% 
  activate(edges) %>% 
  arrange(desc(common_counts))

dev.off()

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

#-----------------------Chinese----------------------------
nodes_chi <- 
  burial_three_period_tidy %>% 
  filter(Phase == "chi") %>% 
  select(burial_label) %>% 
  rowid_to_column("id")

# pair wise combinations for burials as index for later map fuction
burial_comb_chi = t(combn(nodes_chi$burial_label, 2))
colnames(burial_comb_chi) = c("burial_1", "burial_2")
burial_comb_chi = as_tibble(burial_comb_chi)

edge_list_chi <-
  burial_three_period_tidy %>% 
  select(burial_label, 19:35) %>% 
  pivot_longer(-burial_label, names_to = "goods", values_to = "count") %>% 
  #mutate(burial_connection = rep(unique(burial_label), length.out = length(burial_label)))
  group_by(burial_label) %>% 
  nest() 

# pair wise list 
common_counts_lst_chi <- 
  map2(burial_comb_chi$burial_1,
       burial_comb_chi$burial_2,
       ~bind_cols(
         edge_list_chi %>% 
           filter(burial_label == .x) %>% 
           unnest(data),
         edge_list_chi %>% 
           filter(burial_label == .y) %>% 
           unnest(data)) %>% 
         rowwise() %>% 
         mutate(common_counts = sum(count, count1)))

# count of ornament types in common between each pair of burials 
common_counts_vct_chi <- map_int(common_counts_lst_chi, ~sum(!is.na(.x$common_counts)))

burial_comb_with_common_counts_chi <- 
  burial_comb_chi %>% 
  mutate(common_counts = common_counts_vct_chi)

# change label to ids for node linking
edges_chi <- 
  burial_comb_with_common_counts_chi %>% 
  left_join(nodes_chi, by = c("burial_1" = "burial_label")) %>% 
  rename(from = id) %>% 
  left_join(nodes_chi, by = c("burial_2" = "burial_label")) %>% 
  rename(to = id) 

edges_for_network_chi <- select(edges_chi, from, to, common_counts)

# -------------------network analysis 1-------------------------------
library(network)
burial_network_chi <- 
  network(edges_for_network_chi, 
          vertex.attr = nodes_chi, 
          matrix.type = "edgelist", 
          ignore.eval = FALSE,
          directed=FALSE
  )

# plot
plot(burial_network_chi, vertex.cex = 3)

#--------------------network analysis 2------------------------------
detach(package:network)
rm(burial_network_chi)

library(tidygraph)
library(ggraph)

relation_tidy_chi <- tbl_graph(nodes = nodes_chi, edges = edges_for_network_chi, directed = FALSE)

relation_tidy_chi %>% 
  activate(edges) %>% 
  arrange(desc(common_counts))

dev.off()

ggraph(relation_tidy_chi) + 
  geom_edge_link() + 
  geom_node_point() + 
  theme_graph()

ggraph(relation_tidy_chi, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = common_counts), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = burial_label), repel = TRUE) +
  labs(edge_width = "common item") +
  theme_graph()
