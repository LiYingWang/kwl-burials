# Explore and prepare burial data for network analysis:
# 1. create burial value as a new variable
# 2. examine bead distribution to guide group assignment for making ties in 001 file
library(readxl)
library(tidyverse)
library(here)

burial <- read_excel(here("analysis", "data", "raw_data", "Kiwulan_Burials.xlsx"))

# tidy up data, select and combine parameters
burial_goods_selected <-
  burial %>%
  mutate(Phase = ifelse(Phase == 'euro', 'post', Phase)) %>%
  filter(!is.na(Phase)) %>%
  mutate(Gold_leaf = ifelse(Gold_leaf == "shatter", "1", Gold_leaf),
         Stoneware = ifelse(Stoneware == "base", "1", Stoneware),
         Glass_bead = ifelse(Glass_bead == "shatter", "1", Glass_bead),
         Stamped_ceramic = ifelse(Stamped_ceramic == "cluster", "1", Stamped_ceramic)) %>%
  mutate_at(21:ncol(.), as.numeric) %>%
  janitor::remove_empty(which = "cols") %>%
  mutate(total = rowSums(.[c(21:48, 50, 55, 56)], na.rm = TRUE)) %>% # prestige goods
  mutate(Porcelain = rowSums(.[c(40:48, 56)], na.rm = TRUE)) %>% # B&W, porcelain, kendi
  mutate(Porcelain = ifelse(Porcelain == 0, NA, Porcelain)) %>%
  mutate(Stonewares = rowSums(.[c(50, 55)], na.rm = TRUE)) %>% # stoneware, Anping jars
  mutate(Stonewares = ifelse(Stonewares == 0, NA, Stonewares)) %>%
  mutate(Metal_bangles = rowSums(.[c(26, 28, 29)], na.rm = TRUE)) %>% # different bangles
  mutate(Metal_bangles = ifelse(Metal_bangles == 0, NA, Metal_bangles)) %>%
  mutate(Metal_pendant = rowSums(.[c(34, 36)], na.rm = TRUE)) %>% # different bangles
  mutate(Metal_pendant= ifelse(Metal_pendant == 0, NA, Metal_pendant)) %>%
  mutate(all_glass_bead = rowSums(.[c(23, 24)], na.rm = TRUE)) %>%
  select(ID,
         Phase,
         Degree_axis,
         Golden_bead,
         Agate_bead,
         Glass_bead,
         `Indo-Pacific_bead`,
         all_glass_bead,
         Metal_bangles,
         Small_Metal_ring,
         Metal_pendant,
         Coin,
         Porcelain, #prestige good
         Stonewares, #prestige good
         Gold_leaf, #prestige good
         fish_shape_knit,
         #Bone_plate,
         Bell) # select all trade item

# Step 1: create a new varialbe- burial value
# function to calculate type value (Jorgensen 1992)
type_value <- function(burial_good) {
  value <- length(burial_good)/(length(burial_good)-length(which(is.na(burial_good))))
  return(value)
}

# replace counts with type value and sum them up for each burial
burial_with_type_value <-
  burial_goods_selected %>%
  select(-all_glass_bead, -Degree_axis) %>%
  mutate(across(where(is.numeric),
                ~ifelse(is.na(.), 0, type_value(.)))) %>%
  mutate(burial_value = rowSums(across(where(is.numeric)))) %>%
  select(ID, burial_value)

# take the mean of burial value for the burials with goods
mean_burial_value_zero_rm <-
  burial_with_type_value %>%
  filter(!burial_value == 0)

mean_burial_value <-
  mean(mean_burial_value_zero_rm$burial_value)

middle_burial_value <-
  quantile(burial_with_type_value$burial_value,
           probs = c(0.5))

# distribution plot of burial value
ggplot(burial_with_type_value,
       aes(burial_value)) +
  geom_histogram() +
  geom_vline(xintercept = quantile(burial_with_type_value$burial_value,
                                   probs = c(0.3, 0.6, 0.9)),
             color = "red")

high_burial_value <-
  quantile(burial_with_type_value$burial_value,
           probs = c(0.9))

# burial value as a new variable with four levels
burial_with_type_value_class <-
  burial_with_type_value %>%
  mutate(value_class = case_when(
    burial_value == 0 ~ "none",
    burial_value < mean_burial_value ~ "below-average",
    burial_value >= mean_burial_value & burial_value < high_burial_value ~ "above-average",
    burial_value >= high_burial_value ~ "high",
    TRUE ~ ""))

# Step 2: explore bead distribution
# create long format for bead
burial_beads <-
  burial_goods_selected %>%
  select(ID, Agate_bead, Golden_bead, all_glass_bead) %>%
  pivot_longer(col = ends_with("_bead"),
               names_to = "type",
               values_to = "value") %>%
  mutate(value = ifelse(is.na(value), 0, value))

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

# orientation plot
burial_goods_selected %>%
  ggplot(aes(x = Degree_axis, fill = Phase)) +
  geom_histogram()+
  coord_polar() +
  scale_x_continuous(limits = c(0,360))
