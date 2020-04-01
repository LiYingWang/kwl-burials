library(readxl)
library(tidyverse)
library(here)

# read data
burial <- read_excel(here("analysis", "data", "raw_data", "Kiwulan_Burials.xlsx"))

# combine some periods
burial_three_period_tidy <-
  burial %>%
  rename(burial_label = ID) %>%
  mutate(Phase = ifelse(Phase == 'euro', 'post', Phase)) %>%
  filter(!is.na(Phase)) %>%
  mutate(Gold_leaf = ifelse(Gold_leaf == "shatter", "1", Gold_leaf),
         Stoneware = ifelse(Stoneware == "base", "1", Stoneware),
         Stamped_ceramic = ifelse(Stamped_ceramic == "cluster", "1", Stamped_ceramic)) %>%
  mutate_at(21:ncol(.), as.numeric) %>%
  janitor::remove_empty(which = "cols") %>%
  mutate(total = rowSums(.[c(21:48, 50, 55, 56)], na.rm = TRUE)) %>% #prestige goods
  mutate(Porcelain = rowSums(.[c(40:48, 55, 56)], na.rm = TRUE)) %>% #B&W, porcelain, Anping, kendi
  mutate(Porcelain = ifelse(Porcelain == 0, NA, Porcelain)) %>%
  mutate(quantity = case_when(
    total == 0 ~ "none",
    total > 0 & total <= 7 ~ "low",
    total > 7 & total < 100 ~ "medium",
    total >= 100 ~ "high",
    TRUE ~ "other")) %>% # the classification is based on the result of histogram
  mutate(Gold_bead_low = ifelse(Golden_bead == 1, 1, NA),
         Gold_bead_med = ifelse(Golden_bead > 1 & Golden_bead <10, 1, NA),
         Gold_bead_high = ifelse(Golden_bead > 10, 1, NA),
         Agate_bead_low = ifelse(Agate_bead == 1, 1, NA),
         Agate_bead_med = ifelse(Agate_bead > 1 & Agate_bead <10, 1, NA),
         Agate_bead_high = ifelse(Agate_bead > 10, 1, NA),
         `Indo-Pacific_bead_low` = ifelse(`Indo-Pacific_bead` < 100, 1, NA),
         `Indo-Pacific_bead_med` = ifelse(`Indo-Pacific_bead` > 100 & `Indo-Pacific_bead` < 900, 1, NA),
         `Indo-Pacific_bead_high` = ifelse(`Indo-Pacific_bead` > 900, 1, NA)) %>% #based on the result of histogram
  select(burial_label,
         Phase,
         Gold_bead_low,
         Gold_bead_med,
         Gold_bead_high,
         Agate_bead_low,
         Agate_bead_med,
         Agate_bead_high,
         #Agate_bead, #female burials
         #Golden_bead,
         Porcelain,
         Gold_leaf, #prestige good
         fish_shape_knit, #prestige good
         #Bell, #children's burials
         quantity)
         #total) # select specific variable to drop columns (uninformative variables)

# Histogram as a reference for the classification for gold bead and total amount in R script

# number of each phase
burial_three_period_number <-
  burial_three_period_tidy %>%
  count(Phase)

# filter pre burials
burial_pre <-
  burial_three_period_tidy %>%
  filter(Phase == "pre") %>%
  janitor::remove_empty(which = "cols")

# create node list using burial index by phase
#--------------------pre--------------------------------
nodes_pre <-
  burial_three_period_tidy %>%
  filter(Phase == "pre") %>%
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
  burial_three_period_tidy %>%
  select(burial_label, 3:10) %>% # need to change for each exploration
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
       mutate(common_counts = sum(count, count1)))

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
 # %>% mutate(common_counts = ifelse(common_counts > 1, 1, common_counts)) # for unweighted network

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

burial_network_pre <-
  network(edges_for_network_pre, # the network object
          vertex.attr = nodes_pre, # node list
          directed = FALSE, # specify whether the network is directed
          ignore.eval = FALSE, # FALSE = weighted
          loops = FALSE, # do we allow self ties (should not allow them)
          matrix.type = "edgelist") # the type of input

# plot
plot(burial_network_pre, vertex.cex = 1) # It seems the last two nodes are missing?? only 27 nodes

#-----------------------Bayesian ------------------------------
library(statnet)
library(Bergm)

# Add attributes to the network object burial_network_pre
set.vertex.attribute(burial_network_pre, "quantity", burial_pre$quantity)

# plot
set.seed(30)
quantity <- get.vertex.attribute(burial_network_pre, "quantity")
ID <- get.vertex.attribute(burial_network_pre, "burial_label") # not sure how to get id on the network plot
plot(burial_network_pre,
     vertex.col = "quantity",
     vertex.cex = 1.5)

legend("topleft",
       col = c(2, 3, 1, 4), # need to adjust each time
       pch    = 20,
       legend = unique(quantity),
       title  = 'Burial good counts')

# ? can't get the items in legend in order
# ? tried as.factor and set their level but does not work
# ? can't match color with categories, using an odd method at the moment

#------------------creating ERGM model-------------------------------------
# predict ties, as function of network structures,
# endogenous statistics that ties are modelled based on the existence of other ties
# exogenous statistics that ties are formed according to monadic or dyadic node attributes
# Kiwulan case
# https://www.r-bloggers.com/ergm-tutorial/

# every term in an ERGM must have an associated algorithm for computing its value for network
# Morris et al. 2008
model.ergm <- burial_network_pre ~
  edges + # ties, a measure of density, equal to kstar(1) for undirected networks
  density +
  gwdegree(0.5, fixed = TRUE)  +
  triangle # triad relation, a measure of clustering or cohesion, also called transitive triple in undirected network
summary(model.ergm)

# ERGM- consider transitivity by taking into account high-order k-triangles. Triangles and higher order cycles
# gwesp (geometrically weighted edgewise shared partners)
# 1. dyad-based configurations not node-based that counts the number of times that both nodes have a tie to a third node.
# 2. measure of the tendency for two actors who are tied to have x partners in common.
# gwdegree (geometrically weighted degree distribution), a parametric form that represents the frequency distribution for nodal degree, each node counts only once
# Degrees and stars are equivalent representations for the distribution of node-based edge counts

model.2 <- burial_network_pre ~ edges + # density
  gwesp(0.2, fixed = TRUE) +  # transitivity(cohesion; triangle), a tendency for those with shared partners to become tied, or tendency of ties to cluster together
  # triad-based clustering according to dyad-based configurations instead of node-based
  # number means weight parameter alpha, which controls the rate of declining marginal returns
  # fixed = TRUE means the scale parameter lambda is fit as a curved exponential-family model
  # not much difference in a range of 0-1.5, the lower the value of the scaling parameter, the less likely the model is to be degenerate
  # ergm can estimate the parameter from the data by using fixed=FALSE
  # http://mailman13.u.washington.edu/pipermail/statnet_help/2010/000575.html
  gwdegree(0.8, fixed = TRUE)  # popularity(degree; star), the frequency distribution for nodal degrees
  # tendency of being in contact with multiple partners, measures of centralisation
  # distribution of node-based edge counts, each node counts only once
  # number means weight parameter decay
  # Morris et al. (2008)
summary(model.2)

#--------------------Bayesian inference for ERGMs-------------------------
model.3 <- burial_network_pre ~ edges +  # the overall density of the network
  nodematch('quantity') +    # quantity-based homophily, categorical nodal attribute
  # the similarity of connected nodes
  gwesp(0.2, fixed = TRUE) +    # transitivity
  gwdegree(0.8, fixed = TRUE)   # popularity
summary(model.3)

# Specify a prior distribution: normal distribution (low density and high transitivity)
prior.mean <- c(-3, 0, 1, 0) # prior mean corresponds to mean for each parameter
# follow Alberto Caimo et al. (2015) hospital example
prior.sigma <- diag(3, 4, 4) # covariance matrix structure
# normal distribution 𝜃 ∼ Nd (𝜇prior , Σprior ) as a suitable prior model for the model parameters of interests
# where the dimension d corresponds to the number of parameters, 𝜇 is mean vector and Σprior is a d × d covariance matrix.

# Estimated posterior means, medians and 95% credible intervals for Models.3
# bergmM: Bayesian exponential random graphs models under missing data using the approximate exchange algorithm
parpost <- bergm(model.3,
                  prior.mean  = prior.mean,
                  prior.sigma = prior.sigma,
                  burn.in     = 200, # burn-in iterations for every chain of the populationm, drops the first 200
                  main.iters  = 2000, # iterations for every chain of the population
                  aux.iters   = 10000, # MCMC steps used for network simulation
                  nchains     = 8, # number of chains of the population MCMC
                  gamma       = 0.7) # scalar; parallel adaptive direction sampling move factor, acceptance rate

summary(parpost) # Each θ corresponds to the parameter specified in ERGM previously
# In general, positive mean indicates postive correlation, while negative mean indicates negative correlation
# there is different statistics between weighted and unweighted ties
# θ1 = number of ties, θ2 = individuals with the same abundance of burial goods
# θ3 = gwesp is negative that rejects the assumption that actors with multiple partners in common are more likely to be directed connected
# θ4 =

plot(parpost)

# estimate the parameter posterior distribution using the pseudo-posterior calibration approach
# this will take less than the exchange algorithm

parpost2 <- bergmC(model.3,
                   prior.mean  = prior.mean,
                   prior.sigma = prior.sigma,
                   iters       = 100,
                   aux.iters   = 10000,
                   noisy.nsim  = 1000,
                   noisy.thin  = 1000,
                   burnin      = 200,
                   mcmc        = 10000,
                   tunePL      = 1.5) # does not work

# example here
bergmC(formula,
       prior.mean = NULL,
       prior.sigma = NULL,
       burn.in = 10000,
       main.iters = 40000,
       aux.iters = 3000,
       V.proposal = 1.5,
       thin = 1,
       rm.iters = 500,
       rm.a = 0.001,
       rm.alpha = 0, n.aux.draws = 400, aux.thin = 50,
       estimate = c("MLE", "CD"), ...)


# Model assessment, Bayesian goodness of fit diagnostics:
bgof(parpost,
     aux.iters = 10000,
     n.deg     = 14,
     n.dist    = 15,
     n.esp     = 9)

#-------------------------set different parameters------------------------------
# three competing models based on ERGM formulas to fit the data using gwesp and gwdegree

mod.1 <- burial_network_pre ~ edges +
  gwesp(0.2, fixed = TRUE)
summary(mod.1)

mod.2 <- burial_network_pre ~ edges +
  gwdegree(0.8, fixed = TRUE)
summary(mod.2)

mod.3 <- burial_network_pre ~ edges +
  gwesp(0.2, fixed = TRUE) +
  gwdegree(0.8, fixed = TRUE)
summary(mod.3)

mod.sel <- bergm(mod.1,
                 prior.mean  = NULL,
                 prior.sigma = NULL,
                 aux.iters = 100, # auxiliary iterations used for network simulation
                 main.iters = 20, # iterations for every chain of the population
                 burn.in = 200, # burn-in iterations for every chain of the population
                 nchains = 8, # number of chains of the population MCMC
                 gammas = 0.5)

summary(parpost)
plot(parpost)

mod.sel <- bergm(mod.1, iters = 25000,
                 aux.iters = 2000,
                 main.iters = rep(700, 3),
                 burn.ins = rep(100, 3),
                 gammas = c(1, 1, 0.8))
