#---------------------pre-E network diagram using ggraph pkg----------------------------
library(tidygraph)
library(ggraph)

# number of connection based on starting nodes
pre_connection_from <-
  edges_for_network_pre %>%
  mutate(value = ifelse(common_counts > 1, 1, common_counts)) %>%
  mutate(from = as.character(from),
         to = as.character(to)) %>%
  select(-common_counts) %>%
  group_by(from) %>%
  tally()

# number of connection based on ending nodes
pre_connection_to <-
  edges_for_network_pre %>%
  mutate(value = ifelse(common_counts > 1, 1, common_counts)) %>%
  mutate(from = as.character(from),
         to = as.character(to)) %>%
  select(-common_counts) %>%
  group_by(to) %>%
  tally()

# get the total number of connections per burial
pre_connection_per_burial <-
  pre_connection_from %>%
  full_join(pre_connection_to, by= c("from" = "to")) %>%
  rowwise() %>%
  mutate(connections = sum(n.x, n.y, na.rm = TRUE)) %>%
  mutate(from = as.integer(from))

# join total number with original network object
nodes_pre_joined <-
  nodes_pre %>%
  left_join(pre_connection_per_burial, by= c("id" = "from")) %>%
  select(-n.x, -n.y) %>%
  mutate(burial_label_rm = ifelse(is.na(connections), NA, burial_label))

# make network
relation_tidy_pre <- tbl_graph(nodes = nodes_pre_joined,
                               edges = edges_for_network_pre,
                               directed = FALSE)

relation_tidy_pre %>%
  activate(edges) %>%
  arrange(desc(common_counts))

pre_diagram <-
  ggraph(relation_tidy_pre, layout = "stress") + #graphopt
  geom_edge_link(aes(width = as.factor(common_counts)),
                 alpha = 0.5) +
  geom_node_point(aes(filter = !is.na(connections),
                      size = connections,
                      color = connections,
                      alpha = connections)) +
  scale_edge_width_manual(values = c(0.2, 0.5, 1)) +
  #scale_size_continuous(breaks = c(2, 5, 8, 10, 13)) +
  scale_color_viridis(direction = -1) +
  guides(size = guide_legend(), color=guide_legend()) +
  geom_node_text(aes(filter= !is.na(connections) & connections > 11,
                     label = burial_label_rm),
                 repel = TRUE) +
  labs(edge_width = "common items") +
  theme_graph() +
  theme(plot.margin = unit(rep(0.8,4), "cm")) #legend.position="none"

#---------------------post-E network diagram using ggraph pkg----------------------------
# number of connection based on starting nodes
post_connection_from <-
  edges_for_network_post %>%
  mutate(value = ifelse(common_counts > 1, 1, common_counts)) %>%
  mutate(from = as.character(from),
         to = as.character(to)) %>%
  select(-common_counts) %>%
  group_by(from) %>%
  tally()

# number of connection based on ending nodes
post_connection_to <-
  edges_for_network_post %>%
  mutate(value = ifelse(common_counts > 1, 1, common_counts)) %>%
  mutate(from = as.character(from),
         to = as.character(to)) %>%
  select(-common_counts) %>%
  group_by(to) %>%
  tally()

# get the total number of connections per burial
post_connection_per_burial <-
  post_connection_from %>%
  full_join(post_connection_to, by= c("from" = "to")) %>%
  rowwise() %>%
  mutate(connections = sum(n.x, n.y, na.rm = TRUE)) %>%
  mutate(from = as.integer(from))

# join total number with original network object
nodes_post_joined <-
  nodes_post %>%
  left_join(post_connection_per_burial, by= c("id" = "from")) %>%
  select(-n.x, -n.y) %>%
  mutate(burial_label_rm = ifelse(is.na(connections), NA, burial_label))

# make network
relation_tidy_post <- tbl_graph(nodes = nodes_post_joined,
                                edges = edges_for_network_post,
                                directed = FALSE)

relation_tidy_post %>%
  activate(edges) %>%
  arrange(desc(common_counts))

post_diagram <-
  ggraph(relation_tidy_post, layout = "stress") + #graphopt
  geom_edge_link(aes(width = as.factor(common_counts)),
                 alpha = 0.5) +
  geom_node_point(aes(filter= !is.na(connections),
                      size = connections,
                      color = connections,
                      alpha = connections)) +
  scale_edge_width_manual(values=c(0.2, 0.5, 1)) +
  scale_color_viridis_c(direction = -1) +
  guides(size = guide_legend(), color=guide_legend()) +
  geom_node_text(aes(filter= !is.na(connections) & connections > 22,
                     label = burial_label_rm),
                 repel = TRUE) +
  labs(edge_width = "common item") +
  theme_graph() +
  theme(plot.margin = unit(rep(0.8,4), "cm"))

# plot together
library(cowplot)
diagrams_two_phases <-
  plot_grid(pre_diagram,
            post_diagram,
            labels = c('A', 'B'),
            label_size = 12)

ggsave(here::here("analysis", "figures", "005-network-diagrams.png"),
       w = 8, h = 3)
