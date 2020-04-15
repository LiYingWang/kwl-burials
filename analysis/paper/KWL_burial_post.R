#-----------------------European----------------------------
# filter post burials
burial_post <-
  burial_three_period_age_tidy %>%
  filter(Phase == "post") %>%
  janitor::remove_empty(which = "cols")

# create node list
nodes_post <-
  burial_three_period_age_tidy %>%
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
  burial_three_period_age_tidy %>%
  select(burial_label, 5:13) %>% # need to change for each exploration
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
plot(burial_network_post, vertex.cex = 1)

#-----------------------Bayesian ERGMs------------------------------
library(statnet)
library(Bergm)

# Add attributes to the network object burial_network_post
set.vertex.attribute(burial_network_post, "quantity", burial_post$quantity)
set.vertex.attribute(burial_network_post, "age", burial_post$Age_scale)
set.vertex.attribute(burial_network_post, "gender", burial_post$gender)

# plot
set.seed(30)
quantity <- get.vertex.attribute(burial_network_post, "quantity")
age <- get.vertex.attribute(burial_network_post, "age")
ID <- get.vertex.attribute(burial_network_post, "burial_label") # not sure how to get id on the network plot

plot(burial_network_post,
     displaylabels = TRUE,
     vertex.col = "quantity",
     vertex.cex = degree(burial_network_post, cmode = 'indegree') / 12, #size nodes to their in-degree
     #vertex.sides = ifelse(burial_network_pre %v% "", 4, 50),
     pad = 1)

legend("topleft",
       col = c(4, 3, 2, 1), # need to adjust each time
       pch    = 20,
       legend = unique(quantity),
       title  = 'Burial good counts')

#------------------creating ERGM model-------------------------------------
# every term in an ERGM must have an associated algorithm for computing its value for network

model.ergm <- burial_network_post ~
  edges + # ties, a measure of density, equal to kstar(1) for undirected networks
  density +
  gwdegree(0.5, fixed = TRUE)  +
  triangle # triad relation, a measure of clustering or cohesion, also called transitive triple in undirected network
summary(model.ergm)

model.2 <- burial_network_post ~ edges + # density
  gwesp(0.2, fixed = TRUE) +  # transitivity(cohesion; triangle), a tendency for those with shared partners to become tied, or tendency of ties to cluster together
  gwdegree(0.8, fixed = TRUE)  # popularity(degree; star), the frequency distribution for nodal degrees
summary(model.2)

#--------------------Bayesian inference for ERGMs-------------------------
model.3 <- burial_network_post ~ edges +  # the overall density of the network
  nodematch('quantity') +    # quantity-based homophily, the similarity of connected nodes
  gwesp(0.2, fixed = TRUE) +    # transitivity
  gwdegree(0.8, fixed = TRUE)   # popularity
summary(model.3)

# Specify a prior distribution: normal distribution (low density and high transitivity)
prior.mean <- c(-3, 0, 1, 0) # prior mean corresponds to mean for each parameter
prior.sigma <- diag(3, 4, 4) # covariance matrix structure

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

# test for different terms
m1 <- burial_network_post ~ edges +
  nodematch("age") +
  nodematch("quantity") +
  nodematch("gender") +
  gwesp(1.8, fixed = TRUE) + # start close to zero and move up, how well we do in matching the count of triangles
  gwnsp(1.8, fixed = TRUE) +
  gwdegree(0.1, fixed = TRUE)

mod <- bergmM(m1,
              prior.mean  = c(3, 0, 0, 0, 3, -1, 0),
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
     n.deg     = 20,
     n.dist    = 15,
     n.esp     = 25)
