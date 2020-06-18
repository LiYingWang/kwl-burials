library(readxl)
library(tidyverse)
library(here)

burial <- read_excel(here("analysis", "data", "raw_data", "Kiwulan_Burials.xlsx"))

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
  mutate(Porcelain = rowSums(.[c(40:48, 55, 56)], na.rm = TRUE)) %>% #B&W, porcelain, Anping, kendi
  mutate(Porcelain = ifelse(Porcelain == 0, NA, Porcelain)) %>%
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
  mutate(ritual = case_when(
    `Stamped_ceramic` == "2" ~ "two pots",
    `Stamped_ceramic` == "1" ~ "one pot",
    TRUE ~ "NA")) %>%
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
         Age_scale,
         gender,
         ritual, # consider to remove if not very informative
         Gold_bead_low,
         Gold_bead_med,
         Gold_bead_high,
         Agate_bead_low,
         Agate_bead_med,
         Agate_bead_high,
         #Agate_bead, #female burials
         #Golden_bead,
         Porcelain, #prestige good
         Gold_leaf, #prestige good
         fish_shape_knit, #prestige good
         #Bell, #children's burials
         quantity)
#total) # select specific variable to drop columns (uninformative variables)
