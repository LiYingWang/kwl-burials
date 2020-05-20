library(readxl)
library(tidyverse)
library(here)

# read data
burial <- read_excel(here("analysis", "data", "raw_data", "Kiwulan_Burials.xlsx"))

# combine some periods
burial_three_period_tidy <-
  burial %>%
  rename(burial_label = ID) %>%
  mutate(Phase = ifelse(Phase == 'euro', 'post', Phase)) %>%
  filter(!is.na(Phase)) %>%
  mutate(Gold_leaf = ifelse(Gold_leaf == "shatter", "1", Gold_leaf),
         Stoneware = ifelse(Stoneware == "base", "1", Stoneware),
         Stamped_ceramic = ifelse(Stamped_ceramic == "cluster", "1", Stamped_ceramic)) %>%
  mutate_at(21:ncol(.), as.numeric) %>%
  janitor::remove_empty(which = "cols") %>%
  mutate(total = rowSums(.[c(21:48, 50, 55, 56)], na.rm = TRUE)) %>% #prestige goods
  mutate(Porcelain = rowSums(.[c(40:48, 55, 56)], na.rm = TRUE)) %>% #B&W, porcelain, Anping, kendi
  mutate(Porcelain = ifelse(Porcelain == 0, NA, Porcelain)) %>%
  mutate(quantity = case_when(
    total == 0 ~ "none",
    total > 0 & total <= 7 ~ "low",
    total > 7 & total < 100 ~ "medium",
    total >= 100 ~ "high",
    TRUE ~ "other")) %>% # the classification is based on the result of histogram
  mutate(Gold_bead_low = ifelse(Golden_bead == 1, 1, NA),
         Gold_bead_med = ifelse(Golden_bead > 1 & Golden_bead <10, 1, NA),
         Gold_bead_high = ifelse(Golden_bead > 10, 1, NA),
         Agate_bead_low = ifelse(Agate_bead == 1, 1, NA),
         Agate_bead_med = ifelse(Agate_bead > 1 & Agate_bead <10, 1, NA),
         Agate_bead_high = ifelse(Agate_bead > 10, 1, NA),
         `Indo-Pacific_bead_low` = ifelse(`Indo-Pacific_bead` < 100, 1, NA),
         `Indo-Pacific_bead_med` = ifelse(`Indo-Pacific_bead` > 100 & `Indo-Pacific_bead` < 900, 1, NA),
         `Indo-Pacific_bead_high` = ifelse(`Indo-Pacific_bead` > 900, 1, NA)) %>% #based on the result of histogram
  select(burial_label,
         Phase,
         Gold_bead_low,
         Gold_bead_med,
         Gold_bead_high,
         Agate_bead_low,
         Agate_bead_med,
         Agate_bead_high,
         #Agate_bead, #female burials
         #Golden_bead,
         Porcelain,
         Gold_leaf, #prestige good
         fish_shape_knit, #prestige good
         #Bell, #children's burials
         quantity)
#total) # select specific variable to drop columns (uninformative variables)

# filter pre-burials
burial_pre <-
  burial_three_period_tidy %>%
  filter(Phase == "pre") %>%
  janitor::remove_empty(which = "cols")

# get the first 10
burial_pre_small <- burial_pre[1:10,]

# create node list using burial index by phase
#--------------------pre--------------------------------
nodes_pre_small <-
  burial_pre_small %>%
  select(burial_label) %>%
  rowid_to_column("id")

# pair wise combinations for burials as index for later map function
library(gtools)
burial_comb_pre = combinations(length(nodes_pre_small$burial_label), #t(combn(nodes_pre$burial_label, 2))
                               2, nodes_pre_small$burial_label,
                               repeats = TRUE)
colnames(burial_comb_pre) = c("burial_1", "burial_2")
burial_comb_pre = as_tibble(burial_comb_pre)

# create list for each burial that contains the burial good types and their counts
edge_list_pre <-
  burial_pre_small %>%
  select(burial_label, 3:9) %>% # need to change for each exploration
  pivot_longer(-burial_label, names_to = "goods", values_to = "count") %>%
  #mutate(burial_connection = rep(unique(burial_label), length.out = length(burial_label)))
  group_by(burial_label) %>%
  nest()

# pair-wise list
common_counts_lst_pre <-
  map2(burial_comb_pre$burial_1,
       burial_comb_pre$burial_2,
       ~bind_cols(
         edge_list_pre %>%
           filter(burial_label == .x) %>%
           unnest(data),
         edge_list_pre %>%
           filter(burial_label == .y) %>%
           unnest(data)) %>%
         rowwise() %>%
         mutate(common_counts = sum(count, count1)))

# count of ornament types in common between each pair of burials
common_counts_vct_pre <- map_int(common_counts_lst_pre, ~sum(!is.na(.x$common_counts)))

burial_comb_with_common_counts_pre <-
  burial_comb_pre %>%
  mutate(common_counts = common_counts_vct_pre) %>%
  mutate(common_counts = ifelse(burial_1 == burial_2, 0, common_counts)) # use 0 for self loop

# change label to ids for node linking
edges_pre <-
  burial_comb_with_common_counts_pre %>%
  left_join(nodes_pre_small, by = c("burial_1" = "burial_label")) %>%
  rename(from = id) %>%
  left_join(nodes_pre_small, by = c("burial_2" = "burial_label")) %>%
  rename(to = id)

edges_for_network_pre <-
  select(edges_pre, from, to, common_counts) %>%
  filter (!common_counts == 0) # remove rows with no goods in common
# %>% mutate(common_counts = ifelse(common_counts > 1, 1, common_counts)) # for unweighted network
