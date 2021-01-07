# get data from pre-European model
pre_bergm_stats <- as.data.frame(pre_bergm$Theta)
colnames(pre_bergm_stats) <- pre_bergm$specs

pre_bergm_stats <-
  cbind(pre_bergm_stats, phase = "pre") %>%
  rename("gwesp" = "gwesp.fixed.0.4") %>%  #gwnsp" = "gwnsp.fixed.0.4
  rename("dyadcov.dist" = "dyadcov.pre_distance_n.dist")

# get data from post-European model
post_bergm_stats <- as.data.frame(post_bergm$Theta)
colnames(post_bergm_stats) <- post_bergm$specs

post_bergm_stats <-
  cbind(post_bergm_stats, phase = "post") %>%
  rename("gwesp" = "gwesp.fixed.1.2") %>%  #gwnsp" = "gwnsp.fixed.1.2
  rename("dyadcov.dist" = "dyadcov.post_distance_n.dist")

bergm_two_phases <-
  rbind(pre_bergm_stats, post_bergm_stats)

# histogram of posterior distribution for both phases
edges_value <-
  ggplot(bergm_two_phases,
         aes(x = edges, fill = phase)) +
  geom_histogram(alpha = 0.8, position = "identity",
                 binwidth = 0.25) +
  theme_minimal()

trans_value <-
  ggplot(bergm_two_phases,
       aes(x = gwesp, fill = phase)) +
  geom_histogram(alpha = 0.8, position = "identity",
                 binwidth = 0.25) +
  theme_minimal()

degree_value <-
  ggplot(bergm_two_phases,
       aes(x = gwdeg.fixed.0.5, fill = phase)) +
  geom_histogram(alpha = 0.8, position = "identity",
                 binwidth = 0.5) +
  theme_minimal()

dist_value <-
  ggplot(bergm_two_phases,
         aes(x = dyadcov.dist , fill = phase)) +
  geom_histogram(alpha = 0.8, position = "identity",
                 binwidth = 0.004) +
  theme_minimal()

# plot them together
library(cowplot)
histogram <-
  plot_grid(edges_value,
            trans_value,
            degree_value,
            dist_value)

# density plots for posterior distribution
bergm_two_phases_longer <-
  bergm_two_phases %>%
  select(edges, gwesp, gwdeg.fixed.0.5, dyadcov.dist, phase) %>%
  rename(gwdeg = gwdeg.fixed.0.5,
         `distance-dyadcov` = dyadcov.dist) %>%
  pivot_longer(!phase, names_to = "parameter", values_to = "posterior") %>%
  mutate(parameter = factor(parameter,
                            levels=c("edges","gwesp","gwdeg","distance-dyadcov")))

density_posterior_facet <-
  ggplot(bergm_two_phases_longer,
         aes(x = posterior, color = phase, fill = phase))+
  geom_density(alpha= 0.8) +
  facet_wrap(~parameter, scales = "free") +
  theme_minimal()

ggsave(here::here("analysis", "figures", "006-posterior-distribution.png"),
       w = 8, h = 5)

# distribution stats for both phases
distribution_two_phases <-
  rbind(posterior_distribution_pre,
        posterior_distribution_post)

distribution_two_phases_longer <-
  distribution_two_phases %>%
  pivot_longer(cols = starts_with(c("Observed", "Modelled")),
               names_to = "parameter",
               values_to = "value") %>%
  separate(parameter, c("data", "parameter"))

distribution_two_phases_diff <-
  distribution_two_phases %>%
  mutate(degree = abs(Observed.degree-Modelled.degree),
         distance = abs(Observed.distance-Modelled.distance),
         esp = abs(Observed.esp-Modelled.esp)) %>%
  pivot_longer(cols = c("degree", "distance", "esp"),
               names_to = "parameter",
               values_to = "difference")

ggplot(distribution_two_phases_longer,
       aes(parameter, value)) +
  ggpointgrid::geom_pointgrid(aes(color = Phase,
                                  shape = data),
                              size = 3) +
  facet_wrap(~moments, scales = "free") +
  theme_minimal()

ggsave(here::here("analysis", "figures", "006-distribution-moments.png"),
       w = 8, h = 5)
