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
    TRUE ~ "other"))  # the classification is based on the result of histogram

# Histogram to represent the count of burial goods
draw_lines <-
  burial_three_period_tidy_explore %>%
  group_by(quantity) %>%
  summarise(max = min(total)) %>%
  filter(max != 0)

# total burial goods
burial_three_period_tidy_explore %>%
  ggplot(aes(total)) +
  geom_histogram() +
  geom_vline(xintercept = draw_lines$max,
             colour = "red") +
  scale_x_log10()

# gold bead
burial_three_period_tidy_explore %>%
  ggplot(aes(Golden_bead)) +
  geom_histogram() +
  scale_x_log10()

# agate bead
burial_three_period_tidy_explore %>%
  ggplot(aes(Agate_bead)) +
  geom_histogram() +
  scale_x_log10()

# glass bead
burial_three_period_tidy_explore %>%
  ggplot(aes(`Indo-Pacific_bead`)) +
  geom_histogram() +
  scale_x_log10()
