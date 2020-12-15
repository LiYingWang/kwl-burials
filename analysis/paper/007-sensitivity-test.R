# run 006-bergm-bootstrap
#------------------------functions for network stats---------------------------
# function to calculate density using vertex bootstrap
boot_density <- function(igraph_net) {
  rm_matrix <- as.matrix(as_adjacency_matrix(igraph_net))
  boot <- 1000
  set.seed(1)
  pre_5_rm_Astar <- vertboot(rm_matrix, boot)
  pre_5_rm_boot_density <- sapply(1:boot, function(x)
    graph.density(graph_from_adjacency_matrix(pre_5_rm_Astar[[x]])))
  return(pre_5_rm_boot_density)
}

# function to calculate degree using vertex bootstrap
boot_degree <- function(igraph_net) {
  rm_matrix <- as.matrix(as_adjacency_matrix(igraph_net))
  boot <- 1000
  set.seed(1)
  pre_5_rm_Astar <- vertboot(rm_matrix, boot)
  pre_5_rm_boot_degree <- sapply(1:boot, function(x)
    mean(degree(graph_from_adjacency_matrix(pre_5_rm_Astar[[x]]), mode = "in")))
  return(pre_5_rm_boot_degree)
}

# function to calculate transitivity using vertex bootstrap
boot_trans <- function(igraph_net) {
  rm_matrix <- as.matrix(as_adjacency_matrix(igraph_net))
  boot <- 1000
  set.seed(1)
  pre_5_rm_Astar <- vertboot(rm_matrix, boot)
  pre_5_rm_boot_trans <- sapply(1:boot, function(x)
    transitivity(graph_from_adjacency_matrix(pre_5_rm_Astar[[x]]), type = "undirected"))
  return(pre_5_rm_boot_trans)
}

# function for getting stats from observed network
obs_stats_all <- function(igraph_net) {
  obs_stats <- rbind(graph.density(igraph_net),
                     mean(degree(igraph_net)),
                     transitivity(igraph_net))
  return(obs_stats)
}

#-----------------------vertex removal for pre-E-------------------------------------
# remove 5% node (1 node)
pre_5_node_removal <-
  delete_vertices(pre_E_igraph, sample(1:nrow(nodes_pre),
                                       round(nrow(nodes_pre)*0.05),
                                       replace=F)) # remove 1 nodes

# vertex bootstrap for density, degree, and transitivity
pre_5_rm_boot_density <- boot_density(pre_5_node_removal)
pre_5_rm_boot_star <- boot_degree(pre_5_node_removal)
pre_5_rm_boot_trans <- boot_trans(pre_5_node_removal)

# combine 95% CI for three stats and observations
pre_5_rm_boot_all_stats <-
  as.data.frame(rbind(quantile(pre_5_rm_boot_density, c(0.025, 0.975)),
                      quantile(pre_5_rm_boot_star, c(0.025, 0.975)),
                      quantile(pre_5_rm_boot_trans, c(0.025, 0.975)))) %>%
  cbind(obs = obs_stats_all(pre_5_node_removal))

# remove 10% nodes (3 nodes) based on network with 5% nodes removal
pre_10_node_removal <-
  delete_vertices(pre_5_node_removal,
                  sample(1:(nrow(nodes_pre)-round(nrow(nodes_pre)*0.05)),
                         round(nrow(nodes_pre)*0.10-round(nrow(nodes_pre)*0.05)),
                         replace=F)) # remove 3 nodes

# vertex bootstrap for density, degree, and transitivity
pre_10_rm_boot_density <- boot_density(pre_10_node_removal)
pre_10_rm_boot_star <- boot_degree(pre_10_node_removal)
pre_10_rm_boot_trans <- boot_trans(pre_10_node_removal)

# combine 95% CI for three stats and observations
pre_10_rm_boot_all_stats <-
  as.data.frame(rbind(quantile(pre_10_rm_boot_density, c(0.025, 0.975)),
                      quantile(pre_10_rm_boot_star, c(0.025, 0.975)),
                      quantile(pre_10_rm_boot_trans, c(0.025, 0.975)))) %>%
  cbind(obs = obs_stats_all(pre_10_node_removal))

# remove 15% nodes (4 nodes) based on network with 10% nodes removal
pre_15_node_removal <-
  delete_vertices(pre_10_node_removal,
                  sample(1:(nrow(nodes_pre)-round(nrow(nodes_pre)*0.10)),
                         round(nrow(nodes_pre)*0.15-round(nrow(nodes_pre)*0.10)),
                         replace=F)) # remove 3 nodes

# vertex bootstrap for density, degree, and transitivity
pre_15_rm_boot_density <- boot_density(pre_15_node_removal)
pre_15_rm_boot_star <- boot_degree(pre_15_node_removal)
pre_15_rm_boot_trans <- boot_trans(pre_15_node_removal)

# combine 95% CI for three stats and observations
pre_15_rm_boot_all_stats <-
  as.data.frame(rbind(quantile(pre_15_rm_boot_density, c(0.025, 0.975)),
                      quantile(pre_15_rm_boot_star, c(0.025, 0.975)),
                      quantile(pre_15_rm_boot_trans, c(0.025, 0.975)))) %>%
  cbind(obs = obs_stats_all(pre_15_node_removal))

# remove 20% nodes (6 nodes) based on network with 15% nodes removal
pre_20_node_removal <-
  delete_vertices(pre_15_node_removal,
                  sample(1:(nrow(nodes_pre)-round(nrow(nodes_pre)*0.15)),
                         round(nrow(nodes_pre)*0.20-round(nrow(nodes_pre)*0.15)),
                         replace=F)) # remove 3 nodes

# vertex bootstrap for density, degree, and transitivity
pre_20_rm_boot_density <- boot_density(pre_20_node_removal)
pre_20_rm_boot_star <- boot_degree(pre_20_node_removal)
pre_20_rm_boot_trans <- boot_trans(pre_20_node_removal)

# combine 95% CI for three stats and observations
pre_20_rm_boot_all_stats <-
  as.data.frame(rbind(quantile(pre_20_rm_boot_density, c(0.025, 0.975)),
                      quantile(pre_20_rm_boot_star, c(0.025, 0.975)),
                      quantile(pre_20_rm_boot_trans, c(0.025, 0.975)))) %>%
  cbind(obs = obs_stats_all(pre_20_node_removal))

# remove 25% nodes (7 nodes) based on network with 20% nodes removal
pre_25_node_removal <-
  delete_vertices(pre_20_node_removal,
                  sample(1:(nrow(nodes_pre)-round(nrow(nodes_pre)*0.20)),
                         round(nrow(nodes_pre)*0.25-round(nrow(nodes_pre)*0.20)),
                         replace=F)) # remove 3 nodes

# vertex bootstrap for density, degree, and transitivity
pre_25_rm_boot_density <- boot_density(pre_25_node_removal)
pre_25_rm_boot_star <- boot_degree(pre_25_node_removal)
pre_25_rm_boot_trans <- boot_trans(pre_25_node_removal)

# combine 95% CI for three stats and observations
pre_25_rm_boot_all_stats <-
  as.data.frame(rbind(quantile(pre_25_rm_boot_density, c(0.025, 0.975)),
                      quantile(pre_25_rm_boot_star, c(0.025, 0.975)),
                      quantile(pre_25_rm_boot_trans, c(0.025, 0.975)))) %>%
  cbind(obs = obs_stats_all(pre_25_node_removal))

#-----------------------coverage probabilities-------------------------------------
# coverage probabilities
cov_5_mean_all <- rbind(mean(pre_5_rm_boot_density),
                        mean(pre_5_rm_boot_star),
                        mean(pre_5_rm_boot_trans))

cov_5_se_all <- rbind(sd(pre_5_rm_boot_density),
                      sd(pre_5_rm_boot_star),
                      sd(pre_5_rm_boot_trans))

cov_5_den <- sum(pre_5_rm_boot_density <= pre_CIvertboot_den[[2]] &
                   pre_5_rm_boot_density >= pre_CIvertboot_den[[1]])/1000
cov_5_star <- sum(pre_5_rm_boot_star <= pre_CIvertboot_star[[2]] &
                    pre_5_rm_boot_star >= pre_CIvertboot_star[[1]])/1000
cov_5_trans <-sum(pre_5_rm_boot_trans <= pre_CIvertboot_trans[[2]] &
                    pre_5_rm_boot_trans >= pre_CIvertboot_trans[[1]])/1000

pre_5_rm_cov_stats <-
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
