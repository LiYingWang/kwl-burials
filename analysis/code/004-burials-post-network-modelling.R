#-----------------------post-European----------------------------
# filter post burials
burial_post <-
  burial_three_period_age_tidy %>%
  filter(Phase == "post") %>%
  janitor::remove_empty(which = "cols")

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
  select(burial_label, 6:22) %>% # need to change for each exploration
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
  mutate(common_counts = ifelse(burial_1 == burial_2, 0, common_counts)) # 0 for self loop

# change label to ids for node linking
edges_post <-
  burial_comb_with_common_counts_post %>%
  left_join(nodes_post, by = c("burial_1" = "burial_label")) %>%
  rename(from = id) %>%
  left_join(nodes_post, by = c("burial_2" = "burial_label")) %>%
  rename(to = id)

edges_for_network_post <-
  select(edges_post, from, to, common_counts) %>%
  filter (!common_counts == 0) # remove rows without goods in common

#-------------------------create network using network pkg-------------------------------
library(network)

burial_network_post <-
  network(edges_for_network_post, # the network object
          vertex.attr = nodes_post, # node list
          directed = FALSE, # directed or undirected
          ignore.eval = FALSE, # FALSE = weighted
          loops = FALSE, # not allow self ties
          matrix.type = "edgelist") # input type

network.density(burial_network_post)
#-----------------------Bayesian ERGMs------------------------------
library(statnet)
library(Bergm)

# add attributes to the network object burial_network_post
set.vertex.attribute(burial_network_post, "quantity", burial_post$quantity)
set.vertex.attribute(burial_network_post, "age", burial_post$Age_scale)
set.vertex.attribute(burial_network_post, "sex", burial_post$gender)
set.vertex.attribute(burial_network_post, "ritual_pottery", burial_post$ritual_pottery)
set.vertex.attribute(burial_network_post, "value_class", burial_post$value_class)
set.vertex.attribute(burial_network_post, "burial_value", burial_post$burial_value)
set.vertex.attribute(burial_network_post, "orientation", burial_post$orientation)

# get distance matrix, need to run 002 code first
post_distance_n <- network(post_distance, matrix.type = "adjacency", directed = F)
set.edge.attribute(post_distance_n, "dist", post_distance_n)

#------------------creating ERGM model-------------------------------------
# every term in an ERGM must have an associated algorithm for computing its value for network
model_post_1 <- burial_network_post ~
  edges + # ties, a measure of density, equal to kstar(1) for undirected networks
  density +
  triangle + # triad relation, a measure of clustering or cohesion
  gwesp(1.2, fixed = TRUE) + # transitivity(cohesion; triangle), a tendency for nodes with shared partners to cluster together
  gwdegree(0.5, fixed = TRUE)  # popularity(degree; star), nodal degrees

summary(model_post_1)

#--------------------Bayesian inference for ERGMs-------------------------
model_post_3 <- burial_network_post ~
  edges +  # the overall density of the network
  #nodematch('quantity') +  # quantity-based homophily, the similarity of connected nodes
  nodematch('age') +
  nodematch('sex') +
  nodematch('ritual_pottery') +
  nodematch('value_class') +
  #nodematch('orientation') +
  #absdiff('burial_value') +
  gwesp(1.2, fixed = TRUE) + # start from zero and move up to match the count of triangles
  #gwnsp(1.2, fixed = TRUE) +
  gwdegree(0.5, fixed = TRUE) +
  dyadcov(post_distance_n, "dist")

summary(model_post_3)

# Specify a prior distribution
# normal distribution (low density, low transitivity, high popularity)
# priors below follow the order of variables (without#) specified in model 3 (lines 126-138)
post_prior_mean <- c(-3, 0, 0, 0, 3, 1, 3, 0) # prior mean corresponds to mean for each parameter
post_prior_sigma <- diag(c(1, 5, 5, 5, 1, 1, 1, 5), 8, 8) # covariance matrix structure

post_bergm <- bergm(model_post_3,
                 prior.mean  = post_prior_mean ,
                 prior.sigma = post_prior_sigma,
                 burn.in     = 1000, # drops the first 1000
                 main.iters  = 40000, # iterations for every chain of the population
                 aux.iters   = 5000, # MCMC steps used for network simulation
                 nchains     = 32, # number of chains of the population MCMC
                 gamma       = 0) # scalar; parallel adaptive direction sampling move factor, acceptance rate

saveRDS(post_bergm, here::here("analysis", "data", "derived_data", "post_bergm.rds"))
save.image(here::here("analysis", "data", "derived_data", "burial_bergm_model.RData"))

summary(post_bergm)
plot(post_bergm)
