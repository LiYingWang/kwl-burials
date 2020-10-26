#-----------------------post-European----------------------------
# run preparation code in 001-data-tidy.R
# filter post burials
burial_post <-
  burial_three_period_age_tidy %>%
  filter(Phase == "post") %>%
  janitor::remove_empty(which = "cols")
  #filter(quantity != "none") # remove burial without burial goods(49 -> 45)

# create node list
nodes_post <-
  burial_three_period_age_tidy %>%
  filter(Phase == "post") %>%
  #filter(quantity != "none") %>%
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
  burial_three_period_age_tidy %>%
  select(burial_label, 6:15) %>% # need to change for each exploration
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
         mutate(common_counts = sum(count...3, count...6)))

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
plot(burial_network_post, vertex.cex = 1)

#-----------------------Bayesian ERGMs------------------------------
library(statnet)
library(Bergm)

# Add attributes to the network object burial_network_post
set.vertex.attribute(burial_network_post, "quantity", burial_post$quantity)
set.vertex.attribute(burial_network_post, "age", burial_post$Age_scale)
set.vertex.attribute(burial_network_post, "gender", burial_post$gender)
set.vertex.attribute(burial_network_post, "ritual", burial_post$ritual)
set.vertex.attribute(burial_network_post, "value_class", burial_post$value_class)
set.vertex.attribute(burial_network_post, "burial_value", burial_post$burial_value)

#get distance matrix, need to run 002 code first
post_distance_n <- network(post_distance, directed = F)
set.edge.attribute(post_distance_n, "dist", post_distance_n)

# plot
set.seed(30)
quantity <- get.vertex.attribute(burial_network_post, "quantity")
age <- get.vertex.attribute(burial_network_post, "age")
ID <- get.vertex.attribute(burial_network_post, "burial_label") # not sure how to get id on the network plot

plot(burial_network_post,
     displaylabels = TRUE,
     vertex.col = "quantity",
     #vertex.cex = degree(burial_network_post, cmode = 'indegree') / 12, #size nodes to their in-degree
     #vertex.sides = ifelse(burial_network_pre %v% "", 4, 50),
     pad = 1)

legend("topleft",
       col = c(3, 2, 1), # need to adjust each time
       pch    = 20,
       legend = unique(quantity),
       title  = 'Burial goods quantity')

#------------------creating ERGM model-------------------------------------
# every term in an ERGM must have an associated algorithm for computing its value for network
model.ergm <- burial_network_post ~
  edges + # ties, a measure of density, equal to kstar(1) for undirected networks
  density +
  gwdegree(0.5, fixed = TRUE)  +
  triangle # triad relation, a measure of clustering or cohesion, also called transitive triple in undirected network
summary(model.ergm)

model.2 <- burial_network_post ~ edges + # density
  gwesp(1.7, fixed = TRUE) +  # transitivity(cohesion; triangle), a tendency for those with shared partners to become tied, or tendency of ties to cluster together
  gwdegree(0.8, fixed = TRUE)  # popularity(degree; star), the frequency distribution for nodal degrees
summary(model.2)

#--------------------Bayesian inference for ERGMs-------------------------
model.post.3 <- burial_network_post ~ edges +  # the overall density of the network
  nodematch('quantity') +  # quantity-based homophily, the similarity of connected nodes
  nodematch('age') +
  nodematch('gender') +
  nodematch('ritual') +
  nodematch('value_class') +
  absdiff('burial_value') +
  gwesp(1.7, fixed = TRUE) + # start close to zero and move up, how well we do in matching the count of triangles
  gwnsp(1.7, fixed = TRUE) + # original 1.8
  gwdegree(0.8, fixed = TRUE) +
  edgecov(post_distance_n, "dist")
summary(model.post.3)

# Specify a prior distribution: normal distribution (low density and high transitivity)
prior.mean <- c(-3, 1, -1, 0, 0, 1, 1, 1, -1, 1, -1) # prior mean corresponds to mean for each parameter
prior.sigma <- diag(3, 11, 11) # covariance matrix structure

post_bergm <- bergmM(model.post.3,
                 prior.mean  = prior.mean,
                 prior.sigma = prior.sigma,
                 burn.in     = 200, # burn-in iterations for every chain of the population, drops the first 200
                 main.iters  = 2000, # iterations for every chain of the population
                 aux.iters   = 10000, # MCMC steps used for network simulation
                 nchains     = 6, # number of chains of the population MCMC
                 gamma       = 0.1) # scalar; parallel adaptive direction sampling move factor, acceptance rate

summary(post_bergm)

plot(post_bergm)

# Model assessment, Bayesian goodness of fit diagnostics:
bgof_post <-
  bgof(post_bergm,
       aux.iters = 10000,
       n.deg     = 40,
       n.dist    = 15,
       n.esp     = 35)

summary(bgof_post)

# calculate moments for observed and simulated GOF distribution
obs_degree_post <- describe(bgof_post$obs.degree * 0:(length(bgof_post$obs.degree)-1))
obs_dist_post <- describe(bgof_post$obs.dist * 1:length(bgof_post$obs.dist))
obs_esp_post <- describe(bgof_post$obs.esp * 0:(length(bgof_post$obs.esp)-1))

sim_degree_post <-
  describe(bgof_post$sim.degree * rep(0:(length(bgof_post$obs.degree)-1),
                                       times = length(bgof_post$sim.degree)
                                       /length(bgof_post$obs.degree)))
sim_dist_post <-
  describe(bgof_post$sim.dist * rep(1:length(bgof_post$obs.dist),
                                     times = length(bgof_post$sim.dist)
                                     /length(bgof_post$obs.dist)))
sim_esp_post <-
  describe(bgof_post$sim.esp * rep(0:(length(bgof_post$obs.esp)-1),
                                    times = length(bgof_post$sim.esp)
                                    /length(bgof_post$obs.esp)))

# make dataframes for distribution stats
posterior_distribution_post <-
  data.frame("moments" = c("Mean", "Variance", "Skewness"),
             "Observed.degree" = round(c(obs_degree_post$mean,
                                  obs_degree_post$sd,
                                  obs_degree_post$skew), 2),
             "Model.degree" = round(c(mean(sim_degree_post$mean),
                               mean(sim_degree_post$sd),
                               mean(sim_degree_post$skew)),2),
             "Observed.distance" = round(c(obs_dist_post$mean,
                                           obs_dist_post$sd,
                                           obs_dist_post$skew), 2),
             "Model.distance" = round(c(mean(sim_dist_post$mean),
                                        mean(sim_dist_post$sd),
                                        mean(sim_dist_post$skew)),2),
             "Observed.esp" = round(c(obs_esp_post$mean,
                                      obs_esp_post$sd,
                                      obs_esp_post$skew), 2),
             "Model.esp" = round(c(mean(sim_esp_post$mean),
                                   mean(sim_esp_post$sd),
                                   mean(sim_esp_post$skew)),2),
             "Phase" = "post-European")

