# run code in file 000-prep1, 001, and 002 before this file

#-----------------------pre-European-----------------------------
# filter pre burials
burial_pre <-
  burial_three_period_age_tidy %>%
  filter(Phase == "pre") %>%
  janitor::remove_empty(which = "cols")
  #filter(quantity != "none") # remove burial without burial goods

# create node list using burial index by phase
nodes_pre <-
  burial_three_period_age_tidy %>%
  filter(Phase == "pre") %>%
  #filter(quantity != "none") %>% # remove burial without burial goods
  select(burial_label) %>%
  rowid_to_column("id")

# pair wise combinations for burials as index for later map function
library(gtools)
burial_comb_pre = combinations(length(nodes_pre$burial_label),
                               2, nodes_pre$burial_label,
                               repeats = TRUE)
colnames(burial_comb_pre) = c("burial_1", "burial_2")
burial_comb_pre = as_tibble(burial_comb_pre)

# create list for each burial that contains the burial good types and their counts
edge_list_pre <-
  burial_three_period_age_tidy %>%
  select(burial_label, 6:21) %>% # need to adjust if ties change
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
  mutate(common_counts = ifelse(burial_1 == burial_2, 0, common_counts)) # 0 for no self loop

# change label to ids for node linking
edges_pre <-
  burial_comb_with_common_counts_pre %>%
  left_join(nodes_pre, by = c("burial_1" = "burial_label")) %>%
  rename(from = id) %>%
  left_join(nodes_pre, by = c("burial_2" = "burial_label")) %>%
  rename(to = id)

edges_for_network_pre <-
  select(edges_pre, from, to, common_counts) %>%
  filter (!common_counts == 0) # remove rows without goods in common
  # mutate(common_counts = ifelse(common_counts > 1, 1, common_counts)) for unweighted network

#-------------------------create network using network pkg-------------------------------
library(network)

# create network object
attr(edges_for_network_pre, "n") = 29

burial_network_pre <-
  network(edges_for_network_pre, # the network object
          vertex.attr = nodes_pre, # node list
          directed = FALSE, # whether the network is directed
          ignore.eval = FALSE, # FALSE means weighted
          loops = FALSE, # allow self ties or not
          matrix.type = "edgelist") # input type

network.density(burial_network_pre)
#-----------------------attach attributes and make graphs------------------------------
library(statnet)
library(Bergm)

# add attributes to the network object burial_network_pre
set.vertex.attribute(burial_network_pre, "quantity", burial_pre$quantity)
set.vertex.attribute(burial_network_pre, "age", burial_pre$Age_scale)
set.vertex.attribute(burial_network_pre, "sex", burial_pre$gender)
set.vertex.attribute(burial_network_pre, "ritual_pottery", burial_pre$ritual_pottery)
set.vertex.attribute(burial_network_pre, "value_class", burial_pre$value_class) # categorical
set.vertex.attribute(burial_network_pre, "burial_value", burial_pre$burial_value) # numeric
set.vertex.attribute(burial_network_pre, "orientation", burial_pre$orientation)

# get distance matrix, need to run 002 code first
pre_distance_n <- network(pre_distance, matrix.type = "adjacency", directed = F)
set.edge.attribute(pre_distance_n, "dist", pre_distance_n)

#---------------------------create ERGMs-------------------------------------
# model 1, checking triad relations (for clusters) to decide gwesp, Morris et al. (2008)
# check out the terms: http://mailman13.u.washington.edu/pipermail/statnet_help/2010/000575.html
model_pre_1 <- burial_network_pre ~
  edges + # ties, measure density, equal to kstar(1) for undirected networks
  density +
  triangle + # transitive triple in undirected network, measure clustering or cohesion
  gwesp(0.4, fixed = TRUE)+
    # transitivity(cohesion; triangle), a tendency for nodes with shared partners to be tied or clustered
    # number = weight parameter alpha, scaling parameter, controls the rate of declining marginal returns
    # less difference in a range of 0-1.5. The lower the value of the number, the less likely the model is to be degenerate
    # fixed = TRUE means the scale parameter lambda is fit as a curved exponential-family model
    # ergm can estimate the parameter from the data by using fixed=FALSE
  gwdegree(0.3, fixed = TRUE)
    # popularity(degree; star), the frequency distribution for nodal degrees
    # tendency of being in contact with multiple partners, measures of centralization
    # distribution of node-based edge counts, each node counts only once
    # number = weight parameter decay
    # closer to zero, the more gwdegree considers low degree nodes relative to high degree nodes

summary(model_pre_1)

# check edgewise shared partners
summary(burial_network_pre ~ esp(0:10))
summary(burial_network_pre ~ gwdegree(0:10))

# model 3 considers cluster, degree, and node attributes
model_pre_3 <- burial_network_pre ~ edges +  # the overall density of the network
  #nodematch('quantity') + # quantity-based homophily, the similarity of connected nodes
  nodematch('age') +
  nodematch('sex') +
  nodematch('ritual_pottery') +
  nodematch('value_class') +
  #nodematch('orientation') +
  #absdiff('burial_value') + for numeric variable
  gwesp(0.4, fixed = TRUE) + # close to zero and move up to see how well it matches triangles
  #gwnsp(0.4, fixed = TRUE) + # opposite to gwesp
  gwdegree(0.5, fixed = TRUE) +
  dyadcov(pre_distance_n, "dist")
summary(model_pre_3)

#--------------------------Bayesian inference on ERGMs-------------------------
# follow Alberto Caimo et al. (2015) hospital example
# prior uses normal distribution (low density, high transitivity, low popularity)
# need to adjust according to the observed ERGM network
# priors below follow the order of variables (without#) specified in model 3
# positive prior number for edge means high density
pre_prior_mean <- c(-3,   #  edges
                     0,   #  age
                     0,   #  sex
                     3,   #  ritual pottery
                     0,   #  burial value class
                     3,   #  gwesp or transitivity
                    -3,   #  gwdegree or centralization
                     0     #  physical distance
                  )

pre_prior_sigma <- diag(c(1,   #  edges
                          5,   #  age
                          5,   #  sex
                          1,   #  ritual pottery
                          5,   #  burial value class
                          1,   #  gwesp or transitivity
                          1,   #  gwdegree or centralization
                          5),  #  physical distance
                        8, 8) # covariance matrix structure, uncertainty
# normal distribution 𝜃 ∼ Nd (𝜇prior , Σprior ) a common prior model
# where the dimension d corresponds to the number of parameters, 𝜇 is mean vector and Σprior is a d × d covariance matrix
# output includes estimated posterior means, medians and 95% credible intervals

pre_bergm <- bergm(model_pre_3, # using the approximate exchange algorithm
                  prior.mean  = pre_prior_mean,
                  prior.sigma = pre_prior_sigma,
                  burn.in     = 1000, # drop first 1000 for every chain of the population
                  main.iters  = 40000, # iterations for every chain of the population
                  aux.iters   = 5000, # MCMC steps used for network simulation
                  nchains     = 32, # number of chains of the population MCMC
                  gamma       = 0) # scalar; parallel adaptive direction sampling move factor, acceptance rate, 0.2

saveRDS(pre_bergm, here::here("analysis", "data", "derived_data", "pre_bergm.rds"))
summary(pre_bergm)
plot(pre_bergm)
