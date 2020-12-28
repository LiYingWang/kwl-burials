# patchwork bootstrap example
# https://journal.r-project.org/archive/2018/RJ-2018-056/RJ-2018-056.pdf

#----------------------pre-E burials example----------------------------
# make igraph object for pre-E
library(igraph)
pre_E_igraph <- graph_from_data_frame(d = edges_for_network_pre,
                                      vertices = nodes_pre,
                                      directed = FALSE)
pre_density_obs <- graph.density(pre_E_igraph)
pre_mean_star_obs <- mean(degree(pre_E_igraph))
pre_trans_obs <- transitivity(pre_E_igraph) # clustering coefficient
pre_all_obs <- rbind(pre_density_obs,
                     pre_mean_star_obs,
                     pre_trans_obs)
pre_matrix <- as.matrix(as_adjacency_matrix(pre_E_igraph))

# vertex bootstrap, useful for small network
library(snowboot)
set.seed(1)
number <- 1000
pre_Astar <- vertboot(pre_matrix, number)

# calculate 95% bootstrap confidence intervals for the density and bootstrap standard error
pre_boot_density <- sapply(1:number, function(x)
  graph.density(graph_from_adjacency_matrix(pre_Astar[[x]])))
pre_CIvertboot_den <- quantile(pre_boot_density, c(0.025, 0.975))
pre_bootstrap_se_den <- sd(pre_boot_density)

# for mean degree and transitivity
pre_boot_star <- sapply(1:number, function(x)
  mean(degree(graph_from_adjacency_matrix(pre_Astar[[x]]), mode = "in")))
pre_CIvertboot_star <- quantile(pre_boot_star , c(0.025, 0.975))

pre_boot_trans <- sapply(1:number, function(x)
  transitivity(graph_from_adjacency_matrix(pre_Astar[[x]]), type = "undirected"))
pre_CIvertboot_trans <- quantile(pre_boot_trans , c(0.025, 0.975))

pre_boot_all_stats <-
  as.data.frame(rbind(pre_CIvertboot_den,
                      pre_CIvertboot_star,
                      pre_CIvertboot_trans)) %>%
  cbind(obs = pre_all_obs)

# patchwork bootstrap
set.seed(5)
pre_E_igraph_to_network <- igraph_to_network(pre_E_igraph)

# create patches
pre_patches <- lsmi_union(pre_E_igraph_to_network,
                          n.seeds = 15,
                          n.wave = 5)
# estimate mean degree and degree distribution based on single patches
pre_empdd <- lsmi_dd(pre_patches$lsmi_big, pre_E_igraph_to_network)
pre_empdd$mu
pre_empdd$fk

# resampling procedure
pre_bootdd <- boot_dd(pre_empdd,
                      500)# number of bootstrap sample for non-seed vertex

# bootstrap confidence interval
pre_CIpercentile <- boot_ci(pre_bootdd )

plot(pre_empdd)
plot(pre_bootdd)
plot(pre_CIpercentile)

# Cross-validation by selecting the optimal bootstrap confidence interval
pre_CIpatchwork <- lsmi_cv(pre_E_igraph_to_network,
                           n.seeds = c(15, 20, 25),
                           n.wave = 5, B = 500)

# calculate 95% bootstrap confidence intervals for the density
pre_CIpatchwork$bci/(pre_E_igraph_to_network$n - 1)

#----------------------post-E burials example----------------------------
# make igraph object for post-E
post_E_igraph <- graph_from_data_frame(d = edges_for_network_post,
                                       vertices = nodes_post,
                                       directed = FALSE)
post_density_obs <- graph.density(post_E_igraph)
post_mean_star_obs <- mean(degree(post_E_igraph))
post_trans_obs <- transitivity(post_E_igraph)
post_all_obs <- rbind(post_density_obs,
                      post_mean_star_obs,
                      post_trans_obs)
post_matrix <- as.matrix(as_adjacency_matrix(post_E_igraph))

# vertex bootstrap, useful for small network
post_number <- 1000
set.seed(1)
post_Astar <- vertboot(post_matrix, post_number)

# calculate 95% bootstrap confidence intervals for the density and bootstrap standard error
post_boot_density <- sapply(1:post_number, function(x)
    graph.density(graph_from_adjacency_matrix(post_Astar[[x]])))
post_CIvertboot_den <- quantile(post_boot_density, c(0.025, 0.975))
post_CIvertboot_den
post_bootstrap_se <- sd(post_boot_density)
post_bootstrap_se

# confidence interval for mean degree and transitivity
post_boot_star <- sapply(1:number, function(x)
  mean(degree(graph_from_adjacency_matrix(post_Astar[[x]]), mode = "in")))
post_CIvertboot_star <- quantile(post_boot_star , c(0.025, 0.975))

post_boot_trans <- sapply(1:number, function(x)
  transitivity(graph_from_adjacency_matrix(post_Astar[[x]]), type = "undirected"))
post_CIvertboot_trans <- quantile(post_boot_trans , c(0.025, 0.975))

post_boot_all_stats <-
  as.data.frame(rbind(post_CIvertboot_den,
                      post_CIvertboot_star,
                      post_CIvertboot_trans)) %>%
  cbind(obs = post_all_obs)

# patchwork bootstrap
set.seed(5)
post_E_igraph_to_network <- igraph_to_network(post_E_igraph)

# create patches
post_patches <- lsmi_union(post_E_igraph_to_network,
                           n.seeds = 15,
                           n.wave = 5)
# estimate mean degree and degree distribution
post_empdd <- lsmi_dd(post_patches$lsmi_big, post_E_igraph_to_network)
post_empdd$mu
post_empdd$fk

# resampling procedure
post_bootdd <- boot_dd(post_empdd,
                       500)# number of bootstrap sample for non-seed vertex

# bootstrap confidence interval
post_CIpercentile <- boot_ci(post_bootdd )

plot(post_empdd)
plot(post_bootdd)
plot(post_CIpercentile)

# Cross-validation by selecting the optimal bootstrap confidence interval
post_CIpatchwork <- lsmi_cv(post_E_igraph_to_network,
                            n.seeds = c(15, 20, 25),
                            n.wave = 5, B = 500)

# calculate 95% bootstrap confidence intervals for the density
post_CIpatchwork$bci/(post_E_igraph_to_network$n - 1)

#----------------------T test for pre and post network statistics---------------------
# T test for network density, clustering, and degree
t.test(pre_boot_density, post_boot_density)
t.test(pre_boot_trans, post_boot_trans)
t.test(pre_boot_star, post_boot_star)

t.test(post_rm_boot_density, post_boot_density)
t.test(post_rm_boot_trans, post_boot_trans)
t.test(post_rm_boot_star, post_boot_star)
