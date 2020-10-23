# get data from network models
pre_bergm_stats <- as.data.frame(pre_bergm$Theta)
colnames(pre_bergm_stats) <- pre_bergm$specs

pre_bergm_stats <-
  cbind(pre_bergm_stats, phase = "pre")

post_bergm_stats <- as.data.frame(post_bergm$Theta)
colnames(post_bergm_stats) <- post_bergm$specs

post_bergm_stats <-
  cbind(post_bergm_stats, phase = "post")

bergm_two_phases <-
  rbind(pre_bergm_stats, post_bergm_stats)

# making posterior distribution plot for both phases
edges_value <-
  ggplot(bergm_two_phases,
       aes(x = edges, fill = phase)) +
  geom_histogram(position = "identity") +
  theme_minimal()

trans_value <-
  ggplot(bergm_two_phases,
       aes(x = gwesp.fixed.0.75, fill = phase)) +
  geom_histogram(position = "identity") +
  theme_minimal()

degree_value <-
  ggplot(bergm_two_phases,
       aes(x = gwdeg.fixed.0.8, fill = phase)) +
  geom_histogram(position = "identity") +
  theme_minimal()

dist_value <-
  ggplot(bergm_two_phases,
         aes(x = edgecov.dist , fill = phase)) +
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
  pivot_longer(cols = starts_with(c("Observed", "Model")),
               names_to = "parameter",
               values_to = "number") %>%
  separate(parameter, c("value", "parameter"))

ggplot(distribution_two_phases_longer,
       aes(number, parameter)) +
  geom_point(aes(color = Phase, shape = value), size = 3) +
  facet_wrap(~moments, scales = "free")
