# run 006-bergm-bootstrap
#-----------------------vertex removal for post-E-------------------------------------
post_E_node_removal <-
  delete_vertices(post_E_igraph, sample(1:49, 20, replace=F)) # remove 20 nodes

post_rm_density_obs <- graph.density(post_E_node_removal)
post_rm_mean_star_obs <- mean(degree(post_E_node_removal))
post_rm_trans_obs <- transitivity(post_E_node_removal)
post_rm_all_obs <- rbind(post_rm_density_obs,
                         post_rm_mean_star_obs,
                         post_rm_trans_obs)
post_rm_matrix <- as.matrix(as_adjacency_matrix(post_E_node_removal))

# vertex bootstrap, useful for small network
post_number <- 1000
set.seed(1)
post_rm_Astar <- vertboot(post_rm_matrix, post_number)

# calculate 95% bootstrap confidence intervals for the density and bootstrap standard error
post_rm_boot_density <- sapply(1:post_number, function(x)
  graph.density(graph_from_adjacency_matrix(post_rm_Astar[[x]])))
post_rm_CIvertboot_den <- quantile(post_rm_boot_density, c(0.025, 0.975))
post_rm_CIvertboot_den
post_rm_bootstrap_se <- sd(post_rm_boot_density)
post_rm_bootstrap_se

# confedence interval for mean degree and transitivity
post_rm_boot_star <- sapply(1:number, function(x)
  mean(degree(graph_from_adjacency_matrix(post_rm_Astar[[x]]), mode = "in")))
post_rm_CIvertboot_star <- quantile(post_rm_boot_star , c(0.025, 0.975))

post_rm_boot_trans <- sapply(1:number, function(x)
  transitivity(graph_from_adjacency_matrix(post_rm_Astar[[x]]), type = "undirected"))
post_rm_CIvertboot_trans <- quantile(post_rm_boot_trans , c(0.025, 0.975))

post_rm_boot_all_stats <-
  as.data.frame(rbind(post_rm_CIvertboot_den,
                      post_rm_CIvertboot_star,
                      post_rm_CIvertboot_trans)) %>%
  cbind(obs = post_rm_all_obs)
