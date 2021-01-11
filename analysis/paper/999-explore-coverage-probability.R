# code for exploring coverage probabilities and patchwork bootstrap
# need to run 007-bootstrap-CI and 008-bootstrap-t-test first
#-----------------------coverage probabilities-------------------------------------
# coverage probabilities for pre-E
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

# coverage probabilities for post-E
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

#------------------------------patchwork bootstrap for pre-E------------------------------
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

#------------------------------patchwork bootstrap for post-E------------------------------
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
