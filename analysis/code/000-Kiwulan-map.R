library(ggplot2)
theme_set(theme_bw(base_size = 6))
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(here)
library(ggspatial)
#devtools::install_github('3wen/legendMap')
library(legendMap)
library(tmaptools)
library(shadowtext)
library(tidyverse)

world <- ne_countries(scale = "medium", returnclass = "sf")
# class(world)
world_points <- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry))) %>%
  filter(name %in% c("Japan", "China", "Korea", "Taiwan",
                     "Philippines", "Vietnam", "Cambodia"))

# add site location
site_location <-
  data.frame(location = c("Kiwulan","Heping dao", "Tamsui"),
             lon = c(121.7809, 121.7621, 121.4349),
             lat = c(24.8066, 25.1553, 25.1764))

TW_SE_Asia <-
ggplot(data = world) +
  geom_sf() +
  geom_rect(xmin = 120.7, xmax = 122.2, ymin = 24.2, ymax = 25.5,
            fill = NA, colour = "red", size = 0.5) +
  geom_shadowtext(data= world_points,
                  aes(x = X, y = Y,
                      label = name),
                  color='black',
                  bg.colour='white',
                  size = 2,
                  position = position_nudge(y = - 1.7, x = 0.5)) +
  coord_sf(xlim = c(100, 142), ylim = c(7, 43), expand = FALSE) + #add datum = NA to remove
  scale_x_continuous(breaks = seq(100, 140, by = 20)) +
  scale_y_continuous(breaks = seq(15, 40, by = 10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Topographic map
library(ggmap)
# we don't want to download every time, so let's save the map locally
# from https://stackoverflow.com/a/52710855/1036500
tw_map <- ggmap(get_stamenmap(rbind(as.numeric(c(120.7, 24.2,
                                                 122.1, 25.6))), zoom = 10))
#saveRDS(tw_map, here("analysis", "data", "raw_data", "tw_map.rds"))
#tw_map <- readRDS(here("analysis", "data", "raw_data", "tw_map.rds"))
pg <- ggplot_build(tw_map)

TW_map_with_site <-
tw_map +
  geom_point(data = site_location,
             aes(x = lon,
                 y = lat),
             size = 2,
             color = "red") +
  geom_shadowtext(data = site_location,
                  aes(x = lon,
                      y = lat,
                      label = location),
                  size = 2,
                  position = position_nudge(y = 0.06, x = 0.05),
                  check.overlap = TRUE) +
  coord_sf(xlim = c(120.95, 122.05),
           ylim = c(24.4, 25.4),
           expand = FALSE) +
  #scale_x_continuous(breaks = c(121.0, 121.5, 122.0),
                     #limits = c(120.9, 122.7)) +
  legendMap::scale_bar(
    # edit these numbers to select a suitable location
    # for the scale bar where it does not cover
    # important details on the map
    lon = 121.0,
    lat = 24.45,
    legend_size = 2,
    # distance of one section of scale bar, in km
    distance_lon = 20,
    # height of the scale bar, in km
    distance_lat = 1,
    # distance between scale bar and units, in km
    distance_legend = 5,
    # units of scale bar
    dist_unit = "km",
    # add the north arrow
    orientation = TRUE,
    # length of N arrow, in km
    arrow_length = 5,
    # distance between scale bar & base of N arrow, in km
    arrow_distance = 8,
    # size of letter 'N' on N arrow, in km
    arrow_north_size = 4) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggplot() +
  scale_x_continuous(limits = c(0, 2.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  coord_equal() +
  annotation_custom(ggplotGrob(TW_SE_Asia),
                    xmin = 0,
                    xmax = 1.5,
                    ymin = 0,
                    ymax = 1) +
  annotation_custom(ggplotGrob(TW_map_with_site),
                    xmin = 1.2,
                    xmax = 2.5,
                    ymin = 0,
                    ymax = 1) +
  theme_void()

# Figure 1
ggsave(here("analysis", "figures", "000-KWL-map.png"),
       width = 135,
       height = 60,
       dpi = 300,
       units = "mm")
