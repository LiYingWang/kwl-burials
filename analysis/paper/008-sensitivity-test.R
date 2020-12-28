# run 006-bergm-bootstrap
#------------------------functions for network stats---------------------------
# function to calculate density using vertex bootstrap
boot_density <- function(pre_rm_matrix) {
  rm_matrix <- as.matrix(as_adjacency_matrix(igraph_net))
  boot <- 1000
  set.seed(1)
  pre_5_rm_Astar <- vertboot(rm_matrix, boot)
  pre_5_rm_boot_density <- sapply(1:boot, function(x)
    graph.density(graph_from_adjacency_matrix(pre_5_rm_Astar[[x]])))
  return(pre_5_rm_boot_density)
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

# function for getting stats from observed network
obs_stats_all <- function(igraph_net) {
  obs_stats <- rbind(graph.density(igraph_net),
                     transitivity(igraph_net),
                     mean(degree(igraph_net)))
  return(obs_stats)
}

# function for combine 95% CI of three stats and observations
three_CI_and_obs <- function(net_after_node_removal) {
  as.data.frame(rbind(quantile(boot_density(net_after_node_removal), c(0.025, 0.975)),
                      quantile(boot_trans(net_after_node_removal), c(0.025, 0.975)),
                      quantile(boot_degree(net_after_node_removal), c(0.025, 0.975)))) %>%
    cbind(obs = obs_stats_all(net_after_node_removal))
}

#-----------------------vertex removal for pre-E-------------------------------------
# remove 5% node (1 node)
pre_5_node_removal <-
  delete_vertices(pre_E_igraph, sample(1:nrow(nodes_pre),
                                       round(nrow(nodes_pre)*0.05),
                                       replace=F)) # remove 1 nodes

# get 95% CI for three stats and observations
pre_5_rm_boot_all_stats <- three_CI_and_obs(pre_5_node_removal)

# remove 10% nodes (3 nodes) based on network with 5% nodes removal
pre_10_node_removal <-
  delete_vertices(pre_5_node_removal,
                  sample(1:(nrow(nodes_pre)-round(nrow(nodes_pre)*0.05)),
                         round(nrow(nodes_pre)*0.10-round(nrow(nodes_pre)*0.05)),
                         replace=F)) # remove 3 nodes

# get 95% CI for three stats and observations
pre_10_rm_boot_all_stats <- three_CI_and_obs(pre_10_node_removal)

# remove 15% nodes (4 nodes) based on network with 10% nodes removal
pre_15_node_removal <-
  delete_vertices(pre_10_node_removal,
                  sample(1:(nrow(nodes_pre)-round(nrow(nodes_pre)*0.10)),
                         round(nrow(nodes_pre)*0.15-round(nrow(nodes_pre)*0.10)),
                         replace=F))

# get 95% CI for three stats and observations
pre_15_rm_boot_all_stats <- three_CI_and_obs(pre_15_node_removal)

# remove 20% nodes (6 nodes) based on network with 15% nodes removal
pre_20_node_removal <-
  delete_vertices(pre_15_node_removal,
                  sample(1:(nrow(nodes_pre)-round(nrow(nodes_pre)*0.15)),
                         round(nrow(nodes_pre)*0.20-round(nrow(nodes_pre)*0.15)),
                         replace=F))

# get 95% CI for three stats and observations
pre_20_rm_boot_all_stats <- three_CI_and_obs(pre_20_node_removal)

# remove 25% nodes (7 nodes) based on network with 20% nodes removal
pre_25_node_removal <-
  delete_vertices(pre_20_node_removal,
                  sample(1:(nrow(nodes_pre)-round(nrow(nodes_pre)*0.20)),
                         round(nrow(nodes_pre)*0.25-round(nrow(nodes_pre)*0.20)),
                         replace=F))

# get 95% CI for three stats and observations
pre_25_rm_boot_all_stats <- three_CI_and_obs(pre_25_node_removal)

# remove 30% nodes (9 nodes) based on network with 25% nodes removal
pre_30_node_removal <-
  delete_vertices(pre_25_node_removal,
                  sample(1:(nrow(nodes_pre)-round(nrow(nodes_pre)*0.25)),
                         round(nrow(nodes_pre)*0.30-round(nrow(nodes_pre)*0.25)),
                         replace=F))

# get 95% CI for three stats and observations
pre_30_rm_boot_all_stats <- three_CI_and_obs(pre_30_node_removal)

# remove 35% nodes (10 nodes) based on network with 30% nodes removal
pre_35_node_removal <-
  delete_vertices(pre_30_node_removal,
                  sample(1:(nrow(nodes_pre)-round(nrow(nodes_pre)*0.30)),
                         round(nrow(nodes_pre)*0.35-round(nrow(nodes_pre)*0.30)),
                         replace=F))

# get 95% CI for three stats and observations
pre_35_rm_boot_all_stats <- three_CI_and_obs(pre_35_node_removal)

# remove 40% nodes (12 nodes) based on network with 35% nodes removal
pre_40_node_removal <-
  delete_vertices(pre_35_node_removal,
                  sample(1:(nrow(nodes_pre)-round(nrow(nodes_pre)*0.35)),
                         round(nrow(nodes_pre)*0.40-round(nrow(nodes_pre)*0.35)),
                         replace=F))

# get 95% CI for three stats and observations
pre_40_rm_boot_all_stats <- three_CI_and_obs(pre_40_node_removal)

#-----------------------coverage probabilities-------------------------------------
# coverage probabilities
pre_cov_5_mean_all <- rbind(mean(boot_density(pre_5_node_removal)),
                        mean(boot_trans(pre_5_node_removal)),
                        mean(boot_degree(pre_5_node_removal)))

pre_cov_5_se_all <- rbind(sd(boot_density(pre_5_node_removal)),
                      sd(boot_trans(pre_5_node_removal)),
                      sd(boot_degree(pre_5_node_removal)))

pre_cov_5_den <- sum(boot_density(pre_5_node_removal) <= pre_CIvertboot_den[[2]] &
                   boot_density(pre_5_node_removal) >= pre_CIvertboot_den[[1]])/1000
pre_cov_5_trans <- sum(boot_trans(pre_5_node_removal) <= pre_CIvertboot_trans[[2]] &
                    boot_trans(pre_5_node_removal) >= pre_CIvertboot_trans[[1]])/1000
pre_cov_5_star <-sum(boot_degree(pre_5_node_removal) <= pre_CIvertboot_star[[2]] &
                    boot_degree(pre_5_node_removal) >= pre_CIvertboot_star[[1]])/1000

pre_5_rm_cov_stats <-
  as.data.frame(rbind(pre_cov_5_den,
                      pre_cov_5_trans,
                      pre_cov_5_star)) %>%
  cbind(mean = pre_cov_5_mean_all,
        se = pre_cov_5_se_all) %>%
  rename(coverage = V1)

#-----------------------vertex removal for post-E-------------------------------------
# remove 5% node (2 node)
post_5_node_removal <-
  delete_vertices(post_E_igraph, sample(1:nrow(nodes_post),
                                       round(nrow(nodes_post)*0.05),
                                       replace=F)) # remove 2 nodes

# get 95% CI for three stats and observations
post_5_rm_boot_all_stats <- three_CI_and_obs(post_5_node_removal)

# remove 10% nodes (5 nodes) based on network with 5% nodes removal
post_10_node_removal <-
  delete_vertices(post_5_node_removal,
                  sample(1:(nrow(nodes_post)-round(nrow(nodes_post)*0.05)),
                         round(nrow(nodes_post)*0.10-round(nrow(nodes_post)*0.05)),
                         replace=F)) # remove 5 nodes

# get 95% CI for three stats and observations
post_10_rm_boot_all_stats <- three_CI_and_obs(post_10_node_removal)

# remove 15% nodes (7 nodes) based on network with 10% nodes removal
post_15_node_removal <-
  delete_vertices(post_10_node_removal,
                  sample(1:(nrow(nodes_post)-round(nrow(nodes_post)*0.10)),
                         round(nrow(nodes_post)*0.15-round(nrow(nodes_post)*0.10)),
                         replace=F))

# get 95% CI for three stats and observations
post_15_rm_boot_all_stats <- three_CI_and_obs(post_15_node_removal)

# remove 20% nodes (10 nodes) based on network with 15% nodes removal
post_20_node_removal <-
  delete_vertices(post_15_node_removal,
                  sample(1:(nrow(nodes_post)-round(nrow(nodes_post)*0.15)),
                         round(nrow(nodes_post)*0.20-round(nrow(nodes_post)*0.15)),
                         replace=F))

# get 95% CI for three stats and observations
post_20_rm_boot_all_stats <- three_CI_and_obs(post_20_node_removal)

# remove 25% nodes (12 nodes) based on network with 20% nodes removal
post_25_node_removal <-
  delete_vertices(post_20_node_removal,
                  sample(1:(nrow(nodes_post)-round(nrow(nodes_post)*0.20)),
                         round(nrow(nodes_post)*0.25-round(nrow(nodes_post)*0.20)),
                         replace=F))

# get 95% CI for three stats and observations
post_25_rm_boot_all_stats <- three_CI_and_obs(post_25_node_removal)

# remove 30% nodes (15 nodes) based on network with 25% nodes removal
post_30_node_removal <-
  delete_vertices(post_25_node_removal,
                  sample(1:(nrow(nodes_post)-round(nrow(nodes_post)*0.25)),
                         round(nrow(nodes_post)*0.30-round(nrow(nodes_post)*0.25)),
                         replace=F)) # remove 3 nodes

# get 95% CI for three stats and observations
post_30_rm_boot_all_stats <- three_CI_and_obs(post_30_node_removal)

# remove 35% nodes (17 nodes) based on network with 30% nodes removal
post_35_node_removal <-
  delete_vertices(post_30_node_removal,
                  sample(1:(nrow(nodes_post)-round(nrow(nodes_post)*0.30)),
                         round(nrow(nodes_post)*0.35-round(nrow(nodes_post)*0.30)),
                         replace=F)) # remove 3 nodes

# get 95% CI for three stats and observations
post_35_rm_boot_all_stats <- three_CI_and_obs(post_35_node_removal)

# remove 40% nodes (20 nodes) based on network with 35% nodes removal
post_40_node_removal <-
  delete_vertices(post_35_node_removal,
                  sample(1:(nrow(nodes_post)-round(nrow(nodes_post)*0.35)),
                         round(nrow(nodes_post)*0.40-round(nrow(nodes_post)*0.35)),
                         replace=F)) # remove 3 nodes

# get 95% CI for three stats and observations
post_40_rm_boot_all_stats <- three_CI_and_obs(post_40_node_removal)

#-----------------------coverage probabilities-------------------------------------
# coverage probabilities
post_cov_5_mean_all <- rbind(mean(boot_density(post_5_node_removal)),
                        mean(boot_trans(post_5_node_removal)),
                        mean(boot_degree(post_5_node_removal)))

post_cov_5_se_all <- rbind(sd(boot_density(post_5_node_removal)),
                      sd(boot_trans(post_5_node_removal)),
                      sd(boot_degree(post_5_node_removal)))

post_cov_5_den <- sum(boot_density(post_5_node_removal) <= post_CIvertboot_den[[2]] &
                   boot_density(post_5_node_removal) >= post_CIvertboot_den[[1]])/1000
post_cov_5_trans <- sum(boot_trans(post_5_node_removal) <= post_CIvertboot_trans[[2]] &
                     boot_trans(post_5_node_removal) >= post_CIvertboot_trans[[1]])/1000
post_cov_5_star <-sum(boot_degree(post_5_node_removal) <= post_CIvertboot_star[[2]] &
                   boot_degree(post_5_node_removal) >= post_CIvertboot_star[[1]])/1000

post_5_rm_cov_stats <-
  as.data.frame(rbind(post_cov_5_den,
                      post_cov_5_trans,
                      post_cov_5_star)) %>%
  cbind(mean = post_cov_5_mean_all,
        se = post_cov_5_se_all) %>%
  rename(coverage = V1)

#---------------------------network stats visualization-----------------------------------
# combines all network stats in one dataframe
CIs_two_nets <-
  as.data.frame(rbind(pre_0 = three_CI_and_obs(pre_E_igraph),
                      pre_5 = pre_5_rm_boot_all_stats,
                      pre_10 = pre_10_rm_boot_all_stats,
                      pre_15 = pre_15_rm_boot_all_stats,
                      pre_20 = pre_20_rm_boot_all_stats,
                      pre_25 = pre_25_rm_boot_all_stats,
                      pre_30 = pre_30_rm_boot_all_stats,
                      pre_35 = pre_35_rm_boot_all_stats,
                      pre_40 = pre_40_rm_boot_all_stats,
                      post_0 = three_CI_and_obs(post_E_igraph),
                      post_5 = post_5_rm_boot_all_stats,
                      post_10 = post_10_rm_boot_all_stats,
                      post_15 = post_15_rm_boot_all_stats,
                      post_20 = post_20_rm_boot_all_stats,
                      post_25 = post_25_rm_boot_all_stats,
                      post_30 = post_30_rm_boot_all_stats,
                      post_35 = post_35_rm_boot_all_stats,
                      post_40 = post_40_rm_boot_all_stats))

CIs_two_nets$name <- row.names(CIs_two_nets)

CIs_two_nets_tidy <-
  CIs_two_nets %>%
  separate(name, c("phase", "node_net"), sep= "_", remove = T) %>%
  separate(node_net, c("removal", "variable"), sep= "\\.") %>%
  mutate(variable = case_when(
    variable == "1" ~ "density",
    variable == "2" ~ "transitivity",
    variable == "3" ~ "popularity",
    TRUE ~ "")) %>%
  mutate(removal = as.factor(removal))

CIs_two_nets_tidy$removal <-
  factor(CIs_two_nets_tidy$removal,
         levels = c("40", "35", "30", "25", "20", "15", "10", "5", "0"))

CIs_two_nets_tidy %>%
  ggplot(aes(obs,
             removal,
             color = phase,
             group = interaction(phase, removal))) +
  geom_errorbar(aes(xmin = `2.5%`, xmax = `97.5%`),
                width = 0.3,
                position = position_dodge(0.3)) +
  geom_pointrange(aes(xmin = `2.5%`, xmax = `97.5%`),
                  position = position_dodge(0.3)) +
  #scale_color_viridis(discrete= T) +
  theme_minimal() +
  labs(x = "confidensce intervals (95%)",
       y = "node removal (%)") +
  facet_wrap(~variable,
             scales = "free_x")
