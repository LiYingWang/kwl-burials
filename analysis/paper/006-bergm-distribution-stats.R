# get data from pre-European model
pre_bergm_stats <- as.data.frame(pre_bergm$Theta)
colnames(pre_bergm_stats) <- pre_bergm$specs

pre_bergm_stats <-
  cbind(pre_bergm_stats, phase = "pre") %>%
  rename("gwesp" = "gwesp.fixed.0.7") %>%  #gwnsp" = "gwnsp.fixed.0.7
  rename("dyadcov.dist" = "dyadcov.pre_distance_n.dist")

# get data from post-European model
post_bergm_stats <- as.data.frame(post_bergm$Theta)
colnames(post_bergm_stats) <- post_bergm$specs

post_bergm_stats <-
  cbind(post_bergm_stats, phase = "post") %>%
  rename("gwesp" = "gwesp.fixed.1.7") %>%  #gwnsp" = "gwnsp.fixed.1.7
  rename("dyadcov.dist" = "dyadcov.post_distance_n.dist")

bergm_two_phases <-
  rbind(pre_bergm_stats, post_bergm_stats)

# making posterior distribution plot for both phases
edges_value <-
  ggplot(bergm_two_phases,
       aes(x = edges, fill = phase)) +
  geom_histogram(alpha = 0.8, position = "identity") +
  theme_minimal()

trans_value <-
  ggplot(bergm_two_phases,
       aes(x = gwesp, fill = phase)) +
  geom_histogram(alpha = 0.8, position = "identity") +
  theme_minimal()

degree_value <-
  ggplot(bergm_two_phases,
       aes(x = gwdeg.fixed.0.8, fill = phase)) +
  geom_histogram(alpha = 0.8, position = "identity") +
  theme_minimal()

dist_value <-
  ggplot(bergm_two_phases,
         aes(x = dyadcov.dist , fill = phase)) +
  geom_histogram(alpha = 0.8, position = "identity") +
  theme_minimal()

# plot them together
library(cowplot)
plot_grid(edges_value,
          trans_value,
          degree_value,
          dist_value)

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
  ggpointgrid::geom_pointgrid(aes(color = Phase, shape = data), size = 3) +
  facet_wrap(~moments, scales = "free")
