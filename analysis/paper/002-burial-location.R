# create location map and a new variable of geographic distance
library(sf)
library(ggsn)
# read in the spatial data
burial_shape <- invisible(st_read(here("analysis", "data", "raw_data", "shapefiles", "AD_burial.shp"), quiet = TRUE))
post <- invisible(st_read(here("analysis", "data", "raw_data", "shapefiles", "AD_postholes.shp"), quiet = TRUE))
AD <- invisible(st_read(here("analysis", "data", "raw_data", "shapefiles", "AD_zone.shp"), quiet = TRUE))
AD_data <- invisible(st_read(here("analysis", "data", "raw_data", "shapefiles", "AD_withdata.shp"), quiet = TRUE))
location <- invisible(st_read(here("analysis", "data", "raw_data", "shapefiles", "location.shp"), quiet = TRUE))

# add burials
burial_shape_all <-
  burial_shape %>%
  add_row(No_burial = "0",
          geometry = st_sfc(st_polygon(
            list(cbind(c(7042.647, 7022.647, 6900.222, 6930.222, 7042.647),
                       c(-3600.926, -3662.926, -3620.555, -3560.555, -3600.926)))))) %>%
  add_row(No_burial = "12",
          geometry = st_sfc(st_polygon(
            list(cbind(c(6968.425, 6948.256, 6835.349, 6865.666, 6968.425),
                       c(-3480.565, -3537.027, -3490.789, -3440.327, -3480.565)))))) %>%
  add_row(No_burial = "83",
          geometry = st_sfc(st_polygon(
            list(cbind(c(2121.777, 2055.836, 2015.355, 2081.239, 2121.777),
                       c(-2647.565, -2730.027, -2688.789, -2595.327, -2647.565))))))

# join and find centroid points of burials
burial_three_period_age_tidy_geo <-
  burial_shape_all %>%
  right_join(burial_three_period_age_tidy,
            by = c("No_burial" = "burial_label")) %>%
  mutate(burial_cent = st_centroid(geometry)) %>%
  mutate(Period = case_when(
    Phase.y == "pre" ~ "Pre-European",
    Phase.y == "post" ~ "Post-European",
    Phase.y == "chi" ~ "Chinese",
    TRUE ~ ""))

# get distance between pre-E burials
df_pre_distance <-
  burial_three_period_age_tidy_geo %>%
  select(Id, No_burial, Phase.y, geometry, burial_cent) %>%
  mutate(Id = as.numeric(No_burial)) %>%
  filter(Phase.y == "pre") %>%
  arrange(Id)

pre_distance <-
  st_distance(df_pre_distance$burial_cent) # run it in 003

# get distance between post-E burials
df_post_distance <-
  burial_three_period_age_tidy_geo %>%
  select(Id, No_burial, Phase.y, geometry, burial_cent) %>%
  mutate(Id = as.numeric(No_burial)) %>%
  filter(Phase.y == "post") %>%
  arrange(Id)

post_distance <-
  st_distance(df_post_distance$burial_cent) # run it in 004

# mapping burials over the AD section
KWL_burial_map <-
  burial_three_period_age_tidy_geo %>%
  filter(!is.na(geometry)) %>%
  filter(Phase.y != "disturbed") %>%
  ggplot() +
  geom_sf(aes(fill = Period)) +
  geom_sf(data = post, color = 'darkgray', size = 0.1, alpha = 0.4) +
  geom_sf(data = AD_data, fill = NA) +
  scale_fill_viridis_d() +
  theme_minimal() +
  blank() +
  north(AD_data, scale = 0.2, symbol = 3)

ggsave(here::here("analysis", "figures", "002-burial-map.png"),
       w = 8, h = 4)
