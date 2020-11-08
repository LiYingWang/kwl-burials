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
         quantity) +#total) # select specific variable to drop columns (uninformative variables)

# filter pre-burials
burial_pre <-
  burial_three_period_tidy %>%
  filter(Phase == "pre") %>%
  janitor::remove_empty(which = "cols") +
  filter(quantity != "none")


# get the first 15
burial_pre_small <- burial_pre[1:15,]

# create node list using burial index by phase
#--------------------pre--------------------------------
nodes_pre_small <-
  burial_pre_small %>%
  select(burial_label) %>%
  rowid_to_column("id")

# pair wise combinations for burials as index for later map function
library(gtools)
burial_comb_pre = combinations(length(nodes_pre_small$burial_label), #t(combn(nodes_pre$burial_label, 2))
                               2, nodes_pre_small$burial_label,
                               repeats = TRUE)
colnames(burial_comb_pre) = c("burial_1", "burial_2")
burial_comb_pre = as_tibble(burial_comb_pre)

# create list for each burial that contains the burial good types and their counts
edge_list_pre <-
  burial_pre_small %>%
  select(burial_label, 3:9) %>% # need to change for each exploration
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
  left_join(nodes_pre_small, by = c("burial_1" = "burial_label")) %>%
  rename(from = id) %>%
  left_join(nodes_pre_small, by = c("burial_2" = "burial_label")) %>%
  rename(to = id)

edges_for_network_pre <-
  select(edges_pre, from, to, common_counts) %>%
  filter (!common_counts == 0) # remove rows with no goods in common
# %>% mutate(common_counts = ifelse(common_counts > 1, 1, common_counts)) # for unweighted network

#-----------------------Bayesian ERGMs ------------------------------
library(network)
library(statnet)
library(Bergm)

burial_network_pre <-
  network(edges_for_network_pre, # the network object
          vertex.attr = nodes_pre_small, # node list
          directed = FALSE, # specify whether the network is directed
          ignore.eval = FALSE, # FALSE = weighted
          loops = FALSE, # do we allow self ties (should not allow them)
          matrix.type = "edgelist") # the type of input

# Add attributes to the network object burial_network_pre
set.vertex.attribute(burial_network_pre, "quantity", burial_pre$quantity)

# plot
set.seed(30)
quantity <- get.vertex.attribute(burial_network_pre, "quantity")
ID <- get.vertex.attribute(burial_network_pre, "burial_label")
plot(burial_network_pre,
     displaylabels = TRUE,
     vertex.col = "quantity",
     vertex.cex = degree(burial_network_pre, cmode = 'indegree') / 5, #size nodes to their in-degre
     pad = 1) #protects the labels from getting clipped

legend("topleft",
       col = c(2, 3, 1, 4), # need to adjust each time
       pch    = 20,
       legend = unique(quantity),
       title  = 'Burial good counts')

#------------------creating ERGM model-------------------------------------
model.ergm <- burial_network_pre ~
  edges + # ties, a measure of density, equal to kstar(1) for undirected networks
  density +
  gwdegree(0.5, fixed = TRUE)  +
  triangle # triad relation, a measure of clustering or cohesion, also called transitive triple in undirected network
summary(model.ergm)

model.2 <- burial_network_pre ~ edges + # density
  gwesp(0, fixed = TRUE) +
  gwdegree(0.8, fixed = TRUE)
summary(model.2)

#--------------------Bayesian inference for ERGMs-------------------------
model.3 <- burial_network_pre ~ edges +  # the overall density of the network
  nodematch('quantity') +    # quantity-based homophily, categorical nodal attribute
  gwesp(0, fixed = TRUE) + # transitivity
  gwnsp(0.1, fixed = TRUE) +
  gwdegree(0.5, fixed = TRUE)   # popularity
summary(model.3)

# Specify a prior distribution: normal distribution (high density and high transitivity)
prior.mean <- c(-3, 1, 2, -1, 1) # prior mean corresponds to mean for each parameter
prior.sigma <- diag(3, 5, 5) # covariance matrix structure

parpost <- bergm(model.3,
                 prior.mean  = prior.mean,
                 prior.sigma = prior.sigma,
                 burn.in     = 200, # burn-in iterations for every chain of the population, drops the first 200
                 main.iters  = 2000, # iterations for every chain of the population
                 aux.iters   = 10000, # MCMC steps used for network simulation
                 nchains     = 8, # number of chains of the population MCMC
                 gamma       = 0.7) # scalar; parallel adaptive direction sampling move factor, acceptance rate

summary(parpost)
plot(parpost)

# Bayesian Model assessment
bgof <- bgof(parpost,
             aux.iters = 10000,
             n.deg     = 10,
             n.dist    = 10,
             n.esp     = 10)

png("analysis/figures/test/prior111-11.png")
boxplot(bgof$sim.degree)
dev.off()

# try 4 competing models
m1 <- burial_network_pre ~ edges +
  nodematch("age") +
  nodematch("quantity") +
  nodematch("gender") +
  gwesp(0.75, fixed = TRUE) + # start close to zero and move up, how well we do in matching the count of triangles
  gwnsp(0.75, fixed = TRUE) +
  gwdegree(0.2, fixed = TRUE)

m2 <- burial_network_pre ~ edges +
  nodematch("age") +
  gwesp(0.2, fixed = TRUE) +
  gwdegree(0.8, fixed = TRUE)

m3 <- burial_network_pre ~ edges +
  nodematch("quantity") +
  gwesp(0.2, fixed = TRUE) +
  gwdegree(0.8, fixed = TRUE)

m4 <- burial_network_pre ~ edges +
  gwesp(0.2, fixed = TRUE) +
  gwdegree(0.8, fixed = TRUE)

mod <- bergmM(m1,
              prior.mean  = c(2, 0, 0, 0, 4, -1, 0),
              prior.sigma = diag(3, 7, 7),
              burn.in     = 200,
              main.iters  = 2000,
              aux.iters   = 10000,
              nchains     = 8,
              gamma       = 0.2)

summary(mod)

# Model assessment, Bayesian goodness of fit diagnostics:
bgof(mod,
     aux.iters = 10000,
     n.deg     = 15,
     n.dist    = 15,
     n.esp     = 15)

