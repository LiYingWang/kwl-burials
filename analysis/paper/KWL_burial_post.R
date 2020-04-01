#-----------------------European----------------------------
# filter post burials
burial_post <-
  burial_three_period_tidy %>%
  filter(Phase == "post") %>%
  janitor::remove_empty(which = "cols")

# create node list
nodes_post <-
  burial_three_period_tidy %>%
  filter(Phase == "post") %>%
  select(burial_label) %>%
  rowid_to_column("id")

# pair wise combinations for burials as index for later map function
library(gtools)
burial_comb_post = combinations(length(nodes_post$burial_label),
                               2, nodes_post$burial_label,
                               repeats = TRUE)
colnames(burial_comb_post) = c("burial_1", "burial_2")
burial_comb_post = as_tibble(burial_comb_post)

# create list for each burial that contains the burial good types and their counts
edge_list_post <-
  burial_three_period_tidy %>%
  select(burial_label, 3:10) %>% # need to change for each exploration
  pivot_longer(-burial_label, names_to = "goods", values_to = "count") %>%
  group_by(burial_label) %>%
  nest()

# pair-wise list
common_counts_lst_post <-
  map2(burial_comb_post$burial_1,
       burial_comb_post$burial_2,
       ~bind_cols(
         edge_list_post %>%
           filter(burial_label == .x) %>%
           unnest(data),
         edge_list_post %>%
           filter(burial_label == .y) %>%
           unnest(data)) %>%
         rowwise() %>%
         mutate(common_counts = sum(count, count1)))

# count of ornament types in common between each pair of burials
common_counts_vct_post <- map_int(common_counts_lst_post, ~sum(!is.na(.x$common_counts)))

burial_comb_with_common_counts_post <-
  burial_comb_post %>%
  mutate(common_counts = common_counts_vct_post) %>%
  mutate(common_counts = ifelse(burial_1 == burial_2, 0, common_counts)) # use 0 for self loop

# change label to ids for node linking
edges_post <-
  burial_comb_with_common_counts_post %>%
  left_join(nodes_post, by = c("burial_1" = "burial_label")) %>%
  rename(from = id) %>%
  left_join(nodes_post, by = c("burial_2" = "burial_label")) %>%
  rename(to = id)

edges_for_network_post <-
  select(edges_post, from, to, common_counts) %>%
  filter (!common_counts == 0) # remove rows with no goods in common
