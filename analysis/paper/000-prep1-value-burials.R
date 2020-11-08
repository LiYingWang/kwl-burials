library(readxl)
library(tidyverse)
library(here)

burial <- read_excel(here("analysis", "data", "raw_data", "Kiwulan_Burials.xlsx"))

# burial data with combined age and three phases
burial_values <-
  burial %>%
  rename(burial_label = ID) %>%
  mutate(Phase = ifelse(Phase == 'euro', 'post', Phase)) %>%
  filter(!is.na(Phase)) %>%
  mutate(Gold_leaf = ifelse(Gold_leaf == "shatter", "1", Gold_leaf),
         Stoneware = ifelse(Stoneware == "base", "1", Stoneware),
         Glass_bead = ifelse(Glass_bead == "shatter", "1", Glass_bead),
         Stamped_ceramic = ifelse(Stamped_ceramic == "cluster", "1", Stamped_ceramic)) %>%
  mutate_at(21:ncol(.), as.numeric) %>%
  janitor::remove_empty(which = "cols") %>%
  mutate(total = rowSums(.[c(21:48, 50, 55, 56)], na.rm = TRUE)) %>% #prestige goods
  mutate(Porcelain = rowSums(.[c(40:48, 56)], na.rm = TRUE)) %>% #B&W, porcelain, kendi
  mutate(Porcelain = ifelse(Porcelain == 0, NA, Porcelain)) %>%
  mutate(Stonewares = rowSums(.[c(50, 55)], na.rm = TRUE)) %>% #stoneware, Anping jars
  mutate(Stonewares = ifelse(Stonewares == 0, NA, Stonewares)) %>%
  mutate(Metal_bangles = rowSums(.[c(26, 28, 29)], na.rm = TRUE)) %>% #different bangles
  mutate(Metal_bangles = ifelse(Metal_bangles == 0, NA, Porcelain)) %>%
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
  select(burial_label,
         Phase,
         Age_scale,
         gender,
         Golden_bead,
         Agate_bead,
         Glass_bead,
         Metal_bangles,
         Small_Metal_ring,
         `Indo-Pacific_bead`,
         Coin,
         Comb,
         Porcelain, #prestige good
         Stonewares, #prestige good
         Gold_leaf, #prestige good
         fish_shape_knit, #prestige good
         #Bell, #children's burials
         quantity) # select specific variable to drop columns (uninformative variables)

# function for calculate type value (Jorgensen 1992)
type_value <- function(burial_good) {
  value <- length(burial_good)/(length(burial_good)-length(which(is.na(burial_good))))
  return(value)
}

burial_with_type_value <-
  burial_values %>%
  mutate(Golden_bead =
           ifelse(is.na(Golden_bead), 0, type_value(Golden_bead)))

# replace counts with type value and sum up for each burial
burial_with_type_value <-
  burial_values %>%
  mutate(across(where(is.numeric),
           ~ifelse(is.na(.), 0, type_value(.)))) %>%
  mutate(burial_value = rowSums(across(where(is.numeric)))) %>%
  select(burial_label, burial_value)


# distribution plot of burial value
mean(burial_with_type_value$burial_value)
quantile(burial_with_type_value$burial_value)
quantile(burial_with_type_value$burial_value, probs = 0.9)

ggplot(burial_with_type_value,
       aes(burial_value)) +
  geom_histogram() +
  geom_vline(xintercept = c(12, 30), # refers to quantile values
             color = "red")

# divide burial value into classes
burial_with_type_value_class <-
  burial_with_type_value %>%
  mutate(value_class = case_when(
    burial_value == 0 ~ "none",
    burial_value < 12 ~ "low",
    burial_value >= 12 & burial_value < 30 ~ "medium",
    burial_value >= 30 ~ "high",
    TRUE ~ ""))

