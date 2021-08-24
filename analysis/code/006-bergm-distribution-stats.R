# read saved models
load(here::here("analysis", "data", "derived_data", "burial_bergm_model.RData"))

library(cowplot)
library(Bergm)

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
  rename(gwesp = gwesp.fixed.1.2,
         dyadcov.dist = dyadcov.post_distance_n.dist) #gwnsp" = "gwnsp.fixed.1.2

bergm_two_phases <-
  rbind(pre_bergm_stats, post_bergm_stats) %>%
  rename(gwdeg = gwdeg.fixed.0.5,
         `dyadcov.distance` = dyadcov.dist,
         nodematch.ritual = nodematch.ritual_pottery,
         nodematch.value = nodematch.value_class)

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
       aes(x = gwdeg, fill = phase)) +
  geom_histogram(alpha = 0.8, position = "identity",
                 binwidth = 0.5) +
  theme_minimal()

dist_value <-
  ggplot(bergm_two_phases,
         aes(x = dyadcov.distance, fill = phase)) +
  geom_histogram(alpha = 0.8, position = "identity",
                 binwidth = 0.004) +
  theme_minimal()

# plot them together
histogram <-
  plot_grid(edges_value,
            trans_value,
            degree_value,
            dist_value)

# density plots for posterior distribution
bergm_two_phases_longer <-
  bergm_two_phases %>%
  select(edges, gwesp, gwdeg, dyadcov.distance, phase) %>%
  mutate(phase = case_when(
    phase == "pre" ~ "pre-European",
    phase == "post" ~ "post-European",
    TRUE ~ "")) %>%
  rename(transitivity = gwesp,
         centralization = gwdeg,
         "physical distance" = dyadcov.distance) %>%
  pivot_longer(!phase, names_to = "parameter", values_to = "posterior") %>%
  mutate(parameter = factor(parameter,
                            levels=c("edges",
                                     "transitivity",
                                     "centralization",
                                     "physical distance")))

# Figure 5
density_posterior_facet <-
  ggplot(bergm_two_phases_longer,
         aes(x = posterior, fill = phase))+
  geom_density(color = NA, alpha= 0.8) +
  facet_wrap(~parameter, scales = "free") +
  theme_minimal()

ggsave(here::here("analysis", "figures", "006-posterior-distribution.png"), w = 8, h = 5)
