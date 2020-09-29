library(sf)
library(ggsn)
# read in the spatial data
burial_shape <- invisible(st_read(here("analysis", "data", "raw_data", "shapefiles", "AD_burial.shp"), quiet = TRUE))
post <- invisible(st_read(here("analysis", "data", "raw_data", "shapefiles", "AD_postholes.shp"), quiet = TRUE))
AD <- invisible(st_read(here("analysis", "data", "raw_data", "shapefiles", "AD_zone.shp"), quiet = TRUE))
AD_data <- invisible(st_read(here("analysis", "data", "raw_data", "shapefiles", "AD_withdata.shp"), quiet = TRUE))
location <- invisible(st_read(here("analysis", "data", "raw_data", "shapefiles", "location.shp"), quiet = TRUE))

# join and find centroid point of burial
burial_three_period_age_tidy_geo <-
  burial_shape %>%
  left_join(burial_three_period_age_tidy,
            by = c("No_burial" = "burial_label")) %>%
  mutate(burial_cent = st_centroid(geometry))

# mapping burials over the AD section
KWL_burial_map <-
  burial_three_period_age_tidy_geo %>%
  filter(!is.na(geometry)) %>%
  ggplot() +
  geom_sf(aes(fill = Phase.y)) +
  geom_sf(data = post, color = 'blue', size = 0.2, alpha = 0.4) +
  geom_sf(data = AD_data, fill = NA) +
  #geom_sf_text(data = AD_data, aes(label = `坑號`), size = 2.5,
               #alpha = 0.3, fontface = "bold") +
  theme_minimal() +
  blank() +
  north(AD_data, scale = 0.2, symbol = 3)
