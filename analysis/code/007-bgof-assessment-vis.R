# read saved models
load(here::here("analysis", "data", "derived_data", "burial_bergm_model.RData"))
suppressWarnings(source(here::here("analysis", "code", "999-bgof-custom-function.R")))

# Assessment for the two Bayesian models

#------------- Pre-E network
# Figure 3 in the SI
png(filename = here::here("analysis", "figures", "003-pre-bgof.png"),
    width = 5, height = 8, units = "in", res = 360)

library(Bergm)
bgof_pre <-
  bgof2(pre_bergm,
        sample.size = 100,
        aux.iters = 5000,
        n.deg     = 15,
        n.dist    = 15,
        n.esp     = 10)
dev.off()

# load library for measuring moments of distribution
library(psych)

# calculate moments for observed and simulated GOF distribution for pre-E model
obs_degree_pre <- psych::describe(bgof_pre$obs.degree * 0:(length(bgof_pre$obs.degree)-1))
obs_dist_pre <- psych::describe(bgof_pre$obs.dist * 1:length(bgof_pre$obs.dist))
obs_esp_pre <- psych::describe(bgof_pre$obs.esp * 0:(length(bgof_pre$obs.esp)-1))

sim_degree_pre <-
  psych::describe(bgof_pre$sim.degree * rep(0:(length(bgof_pre$obs.degree)-1),
                                     times = length(bgof_pre$sim.degree)
                                     /length(bgof_pre$obs.degree))) %>%
  dplyr::filter(skew != "NaN")

sim_dist_pre <-
  psych::describe(bgof_pre$sim.dist * rep(1:length(bgof_pre$obs.dist),
                                   times = length(bgof_pre$sim.dist)
                                   /length(bgof_pre$obs.dist)))

sim_esp_pre <-
  psych::describe(bgof_pre$sim.esp * rep(0:(length(bgof_pre$obs.esp)-1),
                                  times = length(bgof_pre$sim.esp)
                                  /length(bgof_pre$obs.esp)))
  #filter(skew != "NaN" & skew != 0.00)

# make dataframes for distribution stats
posterior_distribution_pre <-
  data.frame("moments" = c("mean", "variance", "skewness"),
             "observed.degree" = round(c(obs_degree_pre$mean,
                                         obs_degree_pre$sd,
                                         obs_degree_pre$skew), 2),
             "modelled.degree" = round(c(mean(sim_degree_pre$mean),
                                         mean(sim_degree_pre$sd),
                                         mean(sim_degree_pre$skew)),2),
             "observed.distance" = round(c(obs_dist_pre$mean,
                                           obs_dist_pre$sd,
                                           obs_dist_pre$skew), 2),
             "modelled.distance" = round(c(mean(sim_dist_pre$mean),
                                           mean(sim_dist_pre$sd),
                                           mean(sim_dist_pre$skew)),2),
             "observed.esp" = round(c(obs_esp_pre$mean,
                                      obs_esp_pre$sd,
                                      obs_esp_pre$skew), 2),
             "modelled.esp" = round(c(mean(sim_esp_pre$mean),
                                      mean(sim_esp_pre$sd),
                                      mean(sim_esp_pre$skew)),2),
             "phase" = "pre-European")


summary(bgof_pre)

#----------- Post-E network
# Figure 4 in the SI
png(filename = here::here("analysis", "figures", "004-post-bgof.png"),
    width = 5, height = 8, units = "in", res = 360)

bgof_post <-
  bgof2(post_bergm,
        sample.size = 100,
        aux.iters = 5000,
        n.deg     = 30,
        n.dist    = 15,
        n.esp     = 30)

dev.off()

summary(bgof_post)

# calculate moments for observed and simulated GOF distribution for post-E model
obs_degree_post <- describe(bgof_post$obs.degree * 0:(length(bgof_post$obs.degree)-1))
obs_dist_post <- describe(bgof_post$obs.dist * 1:length(bgof_post$obs.dist))
obs_esp_post <- describe(bgof_post$obs.esp * 0:(length(bgof_post$obs.esp)-1))

sim_degree_post <-
  describe(bgof_post$sim.degree * rep(0:(length(bgof_post$obs.degree)-1),
                                      times = length(bgof_post$sim.degree)
                                      /length(bgof_post$obs.degree)))
sim_dist_post <-
  describe(bgof_post$sim.dist * rep(1:length(bgof_post$obs.dist),
                                    times = length(bgof_post$sim.dist)
                                    /length(bgof_post$obs.dist)))
sim_esp_post <-
  describe(bgof_post$sim.esp * rep(0:(length(bgof_post$obs.esp)-1),
                                   times = length(bgof_post$sim.esp)
                                   /length(bgof_post$obs.esp)))

# make dataframes for distribution stats
posterior_distribution_post <-
  data.frame("moments" = c("mean", "variance", "skewness"),
             "observed.degree" = round(c(obs_degree_post$mean,
                                         obs_degree_post$sd,
                                         obs_degree_post$skew), 2),
             "modelled.degree" = round(c(mean(sim_degree_post$mean),
                                         mean(sim_degree_post$sd),
                                         mean(sim_degree_post$skew)),2),
             "observed.distance" = round(c(obs_dist_post$mean,
                                           obs_dist_post$sd,
                                           obs_dist_post$skew), 2),
             "modelled.distance" = round(c(mean(sim_dist_post$mean),
                                           mean(sim_dist_post$sd),
                                           mean(sim_dist_post$skew)),2),
             "observed.esp" = round(c(obs_esp_post$mean,
                                      obs_esp_post$sd,
                                      obs_esp_post$skew), 2),
             "modelled.esp" = round(c(mean(sim_esp_post$mean),
                                      mean(sim_esp_post$sd),
                                      mean(sim_esp_post$skew)),2),
             "phase" = "post-European")

# distribution stats for both phases
distribution_two_phases <-
  rbind(posterior_distribution_pre,
        posterior_distribution_post)

distribution_two_phases_longer <-
  distribution_two_phases %>%
  tidyr::pivot_longer(cols = starts_with(c("observed", "modelled")),
               names_to = "parameter",
               values_to = "value") %>%
  tidyr::separate(parameter, c("data", "parameter"))

distribution_two_phases_diff <-
  distribution_two_phases %>%
  mutate(degree = abs(observed.degree-modelled.degree),
         distance = abs(observed.distance-modelled.distance),
         esp = abs(observed.esp-modelled.esp)) %>%
  pivot_longer(cols = c("degree", "distance", "esp"),
               names_to = "parameter",
               values_to = "difference")

# Figure 7
ggplot(distribution_two_phases_longer,
       aes(parameter, value)) +
  ggpointgrid::geom_pointgrid(aes(color = phase, # https://github.com/nevrome/ggpointgrid
                                  shape = data),
                              size = 3) +
  scale_x_discrete(labels=c("degree",
                            "minimum\ngeodetic\ndistance",
                            "edge-wise\nshared\npartners")) +
  facet_wrap(~moments, scales = "free") +
  theme_minimal()

ggsave(here::here("analysis", "figures", "007-distribution-moments.png"), w = 8, h = 5)

