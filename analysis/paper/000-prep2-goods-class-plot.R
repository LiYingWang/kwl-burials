# this file explores burial data that can be ran individually and separately
# examines distribution of beads to guide group assignment for making ties in 001 file

# tidy up
burial_three_period_tidy_explore <-
  burial %>%
  rename(burial_label = ID) %>%
  mutate(Phase = ifelse(Phase == 'euro', 'post', Phase)) %>%
  filter(!is.na(Phase)) %>%
  mutate(Gold_leaf = ifelse(Gold_leaf == "shatter", "1", Gold_leaf),
         Stoneware = ifelse(Stoneware == "base", "1", Stoneware),
         Stamped_ceramic = ifelse(Stamped_ceramic == "cluster", "1", Stamped_ceramic)) %>%
  mutate_at(21:ncol(.), as.numeric) %>%
  janitor::remove_empty(which = "cols") %>%
  mutate(total = rowSums(.[c(21:48, 50, 55, 56)], na.rm = TRUE)) %>%
  mutate(quantity = case_when(
    total == 0 ~ "none",
    total > 0 & total <= 7 ~ "low",
    total > 7 & total < 100 ~ "medium",
    total >= 100 ~ "high",
    TRUE ~ "other")) %>% # the classification is based on the result of histogram
  rowwise() %>%
  mutate(all_glass_bead = sum(Glass_bead, `Indo-Pacific_bead`, na.rm = TRUE)) %>%
  mutate(Gold_bead_low = ifelse(Golden_bead == 1, 1, NA),
         Gold_bead_med = ifelse(Golden_bead > 1 & Golden_bead <10, 1, NA),
         Gold_bead_high = ifelse(Golden_bead > 10, 1, NA),
         Agate_bead_low = ifelse(Agate_bead == 1, 1, NA),
         Agate_bead_med = ifelse(Agate_bead > 1 & Agate_bead <10, 1, NA),
         Agate_bead_high = ifelse(Agate_bead > 10, 1, NA),
         `Indo-Pacific_bead_low` = ifelse(`Indo-Pacific_bead` < 100, 1, NA),
         `Indo-Pacific_bead_med` = ifelse(`Indo-Pacific_bead` > 100 & `Indo-Pacific_bead` < 900, 1, NA),
         `Indo-Pacific_bead_high` = ifelse(`Indo-Pacific_bead` > 900, 1, NA)) #based on the result of histogram

# long format for bead
burial_beads <-
  burial_three_period_tidy_explore %>%
  select(burial_label, Agate_bead, Golden_bead, Glass_bead, all_glass_bead) %>%
  select(Agate_bead, Golden_bead, all_glass_bead) %>%
  pivot_longer(col = ends_with("_bead"),
               names_to = "type",
               values_to = "value") %>%
  mutate(value = ifelse(is.na(value), 0, value))

# boxplot for all beads
burial_beads %>%
  ggplot(aes(type, value)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_log10()

# density ridgeline plot 1
library(ggridges)
ridge_1 <-
burial_beads %>%
  ggplot(aes(x = value, y = type, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE) +
  scale_x_continuous(limits = c(1, 15), expand = c(0.1, 0)) +
  scale_fill_viridis_d(name = "Quartiles")
  # stat_density_ridges(quantile_lines = TRUE, quantiles = 3)

# density ridgeline plot 2
ridge_2 <-
burial_beads %>%
  ggplot(aes(x = value, y = type, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.3, 0.7)) +
  scale_x_continuous(limits = c(1, 15), expand = c(0.1, 0)) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.3)", "(0.3, 0.7)", "(0.7, 1)")
  )

# density ridgeline plot 3
ridge_3 <-
burial_beads %>%
  ggplot(aes(x = value, y = type, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient",
                      calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability",
                       direction = -1) +
  scale_x_continuous(limits = c(1, 15), expand = c(0.1, 0))

# density ridgeline plot 4
ridge_4 <-
burial_beads %>%
  ggplot(aes(x = value, y = type)) +
  geom_density_ridges(
    jittered_points = TRUE,
    quantile_lines = TRUE,
    scale = 0.9,
    alpha = 0.7,
    vline_size = 1,
    vline_color = "red",
    point_size = 0.4,
    point_alpha = 1,
    position = position_raincloud(adjust_vlines = TRUE)) +
  scale_x_continuous(limits = c(1, 15), expand = c(0.1, 0))

# plot them together for comparison
library(cowplot)
plot_grid(ridge_1, ridge_2, ridge_3, ridge_4,
          ncol = 2)

# orientation
burial %>%
  mutate(Phase = ifelse(Phase == 'euro', 'post', Phase)) %>%
  filter(!is.na(Phase)) %>%
  ggplot(aes(x = Degree_axis, fill = Phase)) +
  geom_histogram()+
  coord_polar() +
  scale_x_continuous(limits = c(0,360))
