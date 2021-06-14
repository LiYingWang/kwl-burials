# need to run 007-bootstrap-CI to get networks
#----------------------vertex bootstrap pre-E burials ----------------------------
# network stats for pre-E
pre_density_obs <- graph.density(pre_E_igraph)
pre_mean_star_obs <- mean(degree(pre_E_igraph))
pre_trans_obs <- transitivity(pre_E_igraph) # clustering coefficient
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

#----------------------vertex bootstrap post-E burials----------------------------
# network stats for post-E
post_density_obs <- graph.density(post_E_igraph)
post_mean_star_obs <- mean(degree(post_E_igraph))
post_trans_obs <- transitivity(post_E_igraph)
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

#----------------------T test for pre and post network statistics---------------------
# T test for network density, clustering, and degree
density_b_test <- t.test(pre_boot_density, post_boot_density)
trans_b_test <- t.test(pre_boot_trans, post_boot_trans)
star_b_test <- t.test(pre_boot_star, post_boot_star)

pval_den <- signif(density_b_test$p.value, 2)
pval_trans <- signif(trans_b_test$p.value, 2)
pval_star <- signif(star_b_test$p.value, 2)
