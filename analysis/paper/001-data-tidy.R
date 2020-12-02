# run 000-prep1 and 000-prep2 first before the code below
# burial data with combined age and three phases
burial_three_period_age_tidy <-
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
  mutate(Porcelain = rowSums(.[c(40:48, 56)], na.rm = TRUE)) %>% #B&W, porcelain, kendi
  mutate(Porcelain = ifelse(Porcelain == 0, NA, Porcelain)) %>%
  mutate(Stonewares = rowSums(.[c(50, 55)], na.rm = TRUE)) %>% #stoneware, Anping jars
  mutate(Stonewares = ifelse(Stonewares == 0, NA, Stonewares)) %>%
  rowwise() %>%
  mutate(all_glass_bead = sum(Glass_bead, `Indo-Pacific_bead`, na.rm = TRUE)) %>%
  mutate(quantity = case_when(
    total == 0 ~ "none",
    total > 0 & total <= 7 ~ "low",
    total > 7 & total < 100 ~ "medium",
    total >= 100 ~ "high",
    TRUE ~ "")) %>% # the classification is based on the result of histogram
  mutate(Age_scale = case_when(
    `Age` %in% c("1","2") ~ "0-12",
    `Age` == "3" ~ "12~20",
    `Age` %in% c("4","5","6","7","8") ~ "+20",
    TRUE ~ "NA")) %>%
  mutate(gender = case_when(
    `Gender` %in% c("1","2") ~ "male",
    `Gender` %in% c("3","4") ~ "female",
    TRUE ~ "NA")) %>%
  mutate(ritual_pottery = case_when(
    `Stamped_ceramic` %in% c("1", "2") ~ "presence",
    TRUE ~ "absense")) %>%
  mutate(orientation = case_when(
    `Degree_axis` %in% c(275:330) ~ "northwest",
    `Degree_axis` %in% c(0:90) ~ "northeast",
    TRUE ~ "NA")) %>%
  mutate(Gold_bead_low = ifelse(Golden_bead == 1, 1, NA),
         Gold_bead_med = ifelse(Golden_bead > 1 & Golden_bead <= 3 , 1, NA),
         Gold_bead_high = ifelse(Golden_bead > 3, 1, NA),
         Agate_bead_low = ifelse(Agate_bead <= 2, 1, NA), # set low level to 2
         Agate_bead_med = ifelse(Agate_bead > 2 & Agate_bead <= 6, 1, NA),
         Agate_bead_high = ifelse(Agate_bead > 6, 1, NA), # set high level to 10
         all_glass_bead_low = ifelse(all_glass_bead > 0 & all_glass_bead <= 2 , 1, NA),
         all_glass_bead_med = ifelse(all_glass_bead > 2 & all_glass_bead <= 6, 1, NA),
         all_glass_bead_high = ifelse(all_glass_bead > 6, 1, NA)) %>% #based on the result of histogram
  left_join(burial_with_type_value_class) %>%
  select(burial_label,
         Phase,
         Age_scale,
         gender,
         ritual_pottery, # consider to remove if not very informative
         Gold_bead_low,
         Gold_bead_med,
         Gold_bead_high,
         Agate_bead_low,
         Agate_bead_med,
         Agate_bead_high,
         all_glass_bead_low,
         all_glass_bead_med,
         all_glass_bead_high,
         #Agate_bead, #female burials
         #Golden_bead,
         Porcelain, #prestige good
         Stonewares, #prestige good
         Gold_leaf, #prestige good
         fish_shape_knit, #prestige good
         #Bell, #children burials
         quantity,
         total,
         burial_value,
         value_class,
         orientation) # select specific variable to drop columns (uninformative variables)

# number of each phase
burial_three_period_age_number <-
  burial_three_period_age_tidy %>%
  count(Phase)

