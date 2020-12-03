#-----------------------pre-European----------------------------
# run preparation code in 001-data-tidy.R
# filter pre burials
burial_pre <-
  burial_three_period_age_tidy %>%
  filter(Phase == "pre") %>%
  janitor::remove_empty(which = "cols")
  #filter(quantity != "none") # remove burial without burial goods

# create node list using burial index by phase
#--------------------pre--------------------------------
nodes_pre <-
  burial_three_period_age_tidy %>%
  filter(Phase == "pre") %>%
  #filter(quantity != "none") %>% # remove burial without burial goods
  select(burial_label) %>%
  rowid_to_column("id")

# pair wise combinations for burials as index for later map function
library(gtools)
burial_comb_pre = combinations(length(nodes_pre$burial_label), #t(combn(nodes_pre$burial_label, 2))
                               2, nodes_pre$burial_label,
                               repeats = TRUE)
colnames(burial_comb_pre) = c("burial_1", "burial_2")
burial_comb_pre = as_tibble(burial_comb_pre)

# create list for each burial that contains the burial good types and their counts
edge_list_pre <-
  burial_three_period_age_tidy %>%
  select(burial_label, 6:17) %>% # need to change for each exploration
  pivot_longer(-burial_label, names_to = "goods", values_to = "count") %>%
  #mutate(burial_connection = rep(unique(burial_label), length.out = length(burial_label)))
  group_by(burial_label) %>%
  nest()

# pair-wise list
common_counts_lst_pre <-
  map2(burial_comb_pre$burial_1,
       burial_comb_pre$burial_2,
       ~bind_cols(
         edge_list_pre %>%
           filter(burial_label == .x) %>%
           unnest(data),
         edge_list_pre %>%
           filter(burial_label == .y) %>%
           unnest(data)) %>%
         rowwise() %>%
         mutate(common_counts = sum(count...3, count...6)))

# count of ornament types in common between each pair of burials
common_counts_vct_pre <- map_int(common_counts_lst_pre, ~sum(!is.na(.x$common_counts)))

burial_comb_with_common_counts_pre <-
  burial_comb_pre %>%
  mutate(common_counts = common_counts_vct_pre) %>%
  mutate(common_counts = ifelse(burial_1 == burial_2, 0, common_counts)) # use 0 for self loop

# change label to ids for node linking
edges_pre <-
  burial_comb_with_common_counts_pre %>%
  left_join(nodes_pre, by = c("burial_1" = "burial_label")) %>%
  rename(from = id) %>%
  left_join(nodes_pre, by = c("burial_2" = "burial_label")) %>%
  rename(to = id)

edges_for_network_pre <-
  select(edges_pre, from, to, common_counts) %>%
  filter (!common_counts == 0) # remove rows with no goods in common
  #mutate(common_counts = ifelse(common_counts > 1, 1, common_counts)) # for unweighted network

#1--------------------network analysis using ggraph pkg------------------------------
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
  geom_edge_link(aes(width = common_counts), alpha = 0.8) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = burial_label), repel = TRUE) +
  labs(edge_width = "common item") +
  theme_graph()

#2-------------------network analysis using network pkg-------------------------------
library(network)

# create network object
attr(edges_for_network_pre, "n") = 29
burial_network_pre <-
  network(edges_for_network_pre, # the network object
          vertex.attr = nodes_pre, # node list
          directed = FALSE, # specify whether the network is directed
          ignore.eval = FALSE, # FALSE = weighted
          loops = FALSE, # do we allow self ties (should not allow them)
          matrix.type = "edgelist") # the type of input

plot(burial_network_pre, vertex.cex = 1)
network.density(burial_network_pre)
network.dyadcount(burial_network_pre, na.omit = TRUE)

#-----------------------attach attributes and make graphs------------------------------
library(statnet)
library(Bergm)

# Add attributes to the network object burial_network_pre
set.vertex.attribute(burial_network_pre, "quantity", burial_pre$quantity)
set.vertex.attribute(burial_network_pre, "age", burial_pre$Age_scale)
set.vertex.attribute(burial_network_pre, "gender", burial_pre$gender)
set.vertex.attribute(burial_network_pre, "ritual_pottery", burial_pre$ritual_pottery)
set.vertex.attribute(burial_network_pre, "value_class", burial_pre$value_class) #categorical
set.vertex.attribute(burial_network_pre, "burial_value", burial_pre$burial_value) #numeric
set.vertex.attribute(burial_network_pre, "orientation", burial_pre$orientation)

#get distance matrix, need to run 002 code first
pre_distance_n <- network(pre_distance, matrix.type = "adjacency", directed = F)
set.edge.attribute(pre_distance_n, "dist", pre_distance_n)

# plot
set.seed(30)
quantity <- get.vertex.attribute(burial_network_pre, "quantity")
age <- get.vertex.attribute(burial_network_pre, "age")
ID <- get.vertex.attribute(burial_network_pre, "burial_label")

plot(burial_network_pre,
     displaylabels = TRUE,
     vertex.col = "quantity",
     vertex.cex = 2,
     #vertex.cex = degree(burial_network_pre, cmode = 'indegree') / 5, #size nodes to their in-degre
     #vertex.sides = ifelse(burial_network_pre %v% "", 4, 50),
     pad = 1) #protects the labels from getting clipped

legend("topleft",
       col = c(2, 3, 1, 4), # need to adjust each time
       pch    = 20,
       legend = unique(quantity),# quantity
       title  = 'Burial good counts')


# ? can't get the items in legend in order, have tried as.factor for level
# can't match color with categories, need to adjust manually every time

#------------------creating ERGM model-------------------------------------
# model 1 considers density and triad relations (for cluster)
model_pre_1 <- burial_network_pre ~
  edges + # ties, a measure of density, equal to kstar(1) for undirected networks
  density +
  gwesp(0.5, fixed = TRUE)  +
  triangle # triad relation, a measure of clustering or cohesion, also called transitive triple in undirected network
summary(model_pre_1)

# Edgewise Shared Partners
summary(burial_network_pre ~ esp(0:10))
summary(burial_network_pre ~ gwdegree(0:10))

# model 2 considers cluster and degree, Morris et al. (2008)
# check out the terms: http://mailman13.u.washington.edu/pipermail/statnet_help/2010/000575.html
model_pre_2 <- burial_network_pre ~ edges + # density
  gwesp(0.5, fixed = TRUE) +
  # transitivity(cohesion; triangle), a tendency for those with shared partners to become tied, or tendency of ties to cluster together
  # number means weight parameter alpha, which controls the rate of declining marginal returns
  # fixed = TRUE means the scale parameter lambda is fit as a curved exponential-family model
  # not much difference in a range of 0-1.5, the lower the value of the scaling parameter, the less likely the model is to be degenerate
  # ergm can estimate the parameter from the data by using fixed=FALSE
  gwdegree(0.3, fixed = TRUE)  # popularity(degree; star), the frequency distribution for nodal degrees
  # tendency of being in contact with multiple partners, measures of centralization
  # distribution of node-based edge counts, each node counts only once
  # number means weight parameter decay
  # The decay is close to zero, the more gwdegree considers low degree nodes relative to high degree nodes
summary(model_pre_2)

# model 3 considers cluster, degree, and node attributes
model_pre_3 <- burial_network_pre ~ edges +  # the overall density of the network
  #nodematch('quantity') + # quantity-based homophily, categorical nodal attribute, the similarity of connected nodes
  nodematch('age') + # prior = -1
  nodematch('gender') + #prior = 0
  nodematch('ritual_pottery') +
  nodematch('value_class') +
  #nodematch('orientation') +
  #absdiff('burial_value') +
  gwesp(0.5, fixed = TRUE) + #start close to zero and move up, how well we do in matching the count of triangles
  #gwnsp(0.8, fixed = TRUE) + #0.75, #prior = -1
  gwdegree(0.3, fixed = TRUE) + # prior = 3
  dyadcov(pre_distance_n, "dist")
summary(model_pre_3)

#--------------------Bayesian inference for ERGMs-------------------------
# prior suggestion: normal distribution (low density and high transitivity), but it also depends on the ERGM netowrk we observed
prior.mean <- c(-3, 0, 0, 1, 0, 3, -2, 0) # positive prior number for edge means high density
# follow Alberto Caimo et al. (2015) hospital example
prior.sigma <- diag(c(5, 3, 3, 3, 3, 5, 3, 3), 8, 8) # covariance matrix structure, uncertainty

# normal distribution 𝜃 ∼ Nd (𝜇prior , Σprior ) a common prior model
# where the dimension d corresponds to the number of parameters, 𝜇 is mean vector and Σprior is a d × d covariance matrix.

# Estimated posterior means, medians and 95% credible intervals for Models.3
# bergmM: Bayesian exponential random graphs models under missing data using the approximate exchange algorithm
pre_bergm <- bergmM(model_pre_3,
                  prior.mean  = prior.mean,
                  prior.sigma = prior.sigma,
                  burn.in     = 100, # drop first 100 for every chain of the population
                  main.iters  = 1000, # iterations for every chain of the population
                  aux.iters   = 3000, # MCMC steps used for network simulation
                  nchains     = 16, # number of chains of the population MCMC
                  gamma       = 0) # scalar; parallel adaptive direction sampling move factor, acceptance rate

summary(pre_bergm)

plot(pre_bergm)

# Bayesian Model assessment
bgof_pre <-
  bgof(pre_bergm,
       aux.iters = 10000,
       n.deg     = 15,
       n.dist    = 15,
       n.esp     = 10)

summary(bgof_pre)

# load library for measuring moments of distribution
library(psych)

# calculate moments for observed and simulated GOF distribution
obs_degree_pre <- describe(bgof_pre$obs.degree * 0:(length(bgof_pre$obs.degree)-1))
obs_dist_pre <- describe(bgof_pre$obs.dist * 1:length(bgof_pre$obs.dist))
obs_esp_pre <- describe(bgof_pre$obs.esp * 0:(length(bgof_pre$obs.esp)-1))

sim_degree_pre <-
  describe(bgof_pre$sim.degree * rep(0:(length(bgof_pre$obs.degree)-1),
                                       times = length(bgof_pre$sim.degree)
                                       /length(bgof_pre$obs.degree)))
sim_dist_pre <-
  describe(bgof_pre$sim.dist * rep(1:length(bgof_pre$obs.dist),
                                     times = length(bgof_pre$sim.dist)
                                     /length(bgof_pre$obs.dist)))
sim_esp_pre <-
  describe(bgof_pre$sim.esp * rep(0:(length(bgof_pre$obs.esp)-1),
                                       times = length(bgof_pre$sim.esp)
                                       /length(bgof_pre$obs.esp))) %>%
  filter(mean != 0.00)

# make dataframes for distribution stats
posterior_distribution_pre <-
  data.frame("moments" = c("Mean", "Variance", "Skewness"),
             "Observed.degree" = round(c(obs_degree_pre$mean,
                                  obs_degree_pre$sd,
                                  obs_degree_pre$skew), 2),
             "Modelled.degree" = round(c(mean(sim_degree_pre$mean),
                               mean(sim_degree_pre$sd),
                               mean(sim_degree_pre$skew)),2),
             "Observed.distance" = round(c(obs_dist_pre$mean,
                                           obs_dist_pre$sd,
                                           obs_dist_pre$skew), 2),
             "Modelled.distance" = round(c(mean(sim_dist_pre$mean),
                                        mean(sim_dist_pre$sd),
                                        mean(sim_dist_pre$skew)),2),
             "Observed.esp" = round(c(obs_esp_pre$mean,
                                      obs_esp_pre$sd,
                                      obs_esp_pre$skew), 2),
             "Modelled.esp" = round(c(mean(sim_esp_pre$mean),
                                   mean(sim_esp_pre$sd),
                                   mean(sim_esp_pre$skew)),2),
             "Phase" = "pre-European")

