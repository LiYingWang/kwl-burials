# run 006-bergm-bootstrap
#-----------------------vertex removal for pre-E-------------------------------------
pre_5_node_removal <-
  delete_vertices(pre_E_igraph, sample(1:nrow(nodes_pre),
                                       round(nrow(nodes_pre)*0.15),
                                       replace=F)) # remove nodes

pre_5_rm_density_obs <- graph.density(pre_5_node_removal)
pre_5_rm_mean_star_obs <- mean(degree(pre_5_node_removal))
pre_5_rm_trans_obs <- transitivity(pre_5_node_removal)
pre_5_rm_all_obs <- rbind(pre_5_rm_density_obs,
                          pre_5_rm_mean_star_obs,
                          pre_5_rm_trans_obs)
pre_5_rm_matrix <- as.matrix(as_adjacency_matrix(pre_5_node_removal))

# vertex bootstrap, useful for small network
pre_rm_number <- 1000
set.seed(1)
pre_5_rm_Astar <- vertboot(pre_5_rm_matrix, pre_rm_number )

# calculate 95% bootstrap confidence intervals for the density and bootstrap standard error
pre_5_rm_boot_density <- sapply(1:pre_rm_number, function(x)
  graph.density(graph_from_adjacency_matrix(pre_5_rm_Astar[[x]])))
pre_5_rm_CIvertboot_den <- quantile(pre_5_rm_boot_density, c(0.025, 0.975))
pre_5_rm_mean_den <- mean(pre_5_rm_boot_density)
pre_5_rm_se_den <- sd(pre_5_rm_boot_density)

# confidence interval for degree
pre_5_rm_boot_star <- sapply(1:number, function(x)
  mean(degree(graph_from_adjacency_matrix(pre_5_rm_Astar[[x]]), mode = "in")))
pre_5_rm_CIvertboot_star <- quantile(pre_5_rm_boot_star, c(0.025, 0.975))
pre_5_rm_mean_star <- mean(pre_5_rm_boot_star)
pre_5_rm_se_star <- sd(pre_5_rm_boot_star)

# confidence interval for transitivity
pre_5_rm_boot_trans <- sapply(1:number, function(x)
  transitivity(graph_from_adjacency_matrix(pre_5_rm_Astar[[x]]), type = "undirected"))
pre_5_rm_CIvertboot_trans <- quantile(pre_5_rm_boot_trans, c(0.025, 0.975))
pre_5_rm_mean_trans <- mean(pre_5_rm_boot_trans)
pre_5_rm_se_trans <- sd(pre_5_rm_boot_trans)

pre_5_rm_boot_all_stats <-
  as.data.frame(rbind(pre_5_rm_CIvertboot_den,
                      pre_5_rm_CIvertboot_star,
                      pre_5_rm_CIvertboot_trans)) %>%
  cbind(obs = pre_5_rm_all_obs)

# coverage probabilities
cov_5_mean_all <- rbind(pre_5_rm_mean_den,
                        pre_5_rm_mean_star,
                        pre_5_rm_mean_trans)

cov_5_se_all <- rbind(pre_5_rm_se_den,
                      pre_5_rm_se_star,
                      pre_5_rm_se_trans)

cov_5_den <- sum(pre_5_rm_boot_density <= pre_CIvertboot_den[[2]] &
                   pre_5_rm_boot_density >= pre_CIvertboot_den[[1]])/1000
cov_5_star <- sum(pre_5_rm_boot_star <= pre_CIvertboot_star[[2]] &
                    pre_5_rm_boot_star >= pre_CIvertboot_star[[1]])/1000
cov_5_trans <-sum(pre_5_rm_boot_trans <= pre_CIvertboot_trans[[2]] &
                    pre_5_rm_boot_trans >= pre_CIvertboot_trans[[1]])/1000

pre_15_rm_cov_stats <-
  as.data.frame(rbind(cov_5_den,
                      cov_5_star,
                      cov_5_trans)) %>%
  cbind(mean = cov_5_mean_all,
        se = cov_5_se_all) %>%
  rename(coverage=V1)

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
