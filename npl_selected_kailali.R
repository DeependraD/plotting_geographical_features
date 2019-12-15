require(sf)
require(tidyverse)

# read nepal district map
np_dist <- sf::st_read("./data/npl_districts/NPL_districts_poly_sd_171123.shp")
np_dist$DISTRICT <- np_dist$DISTRICT %>% fct_recode("CHITWAN" = "CHITAWAN")

# the object has "sf" and "data.frame" class 
np_dist %>% class()

# geometry has "sfc_POLYGON" and "sfc" class
np_dist$geometry %>% class()

# # by default plotting of sf object will produce plots for all attributes (if within allowable maximum)
# plot(np_dist)

# plotting specific attribute only
plot(np_dist["DISTRICT"])
plot(np_dist["DISTRICT"], axes = TRUE)
# plot(np_dist["DISTRICT"], key.pos = 2) # plots attribute with key to the left

# plots graticule (grid lines) too
plot(st_geometry(np_dist), axes = TRUE, graticule = T)

# selecting specific features
np_dist$DISTRICT
kailali_dist <- "KAILALI"

# plotting only selected features
plot(st_geometry(np_dist[np_dist$DISTRICT %in% kailali_dist, ]))

# get geometry properties
sf::st_geometry(np_dist)

# get area of individual features in standard units
sf::st_area(np_dist)

# get bounding box of a set of features 
sf::st_bbox(np_dist)

# base plotting
# plot graticulated map with selected features and their centroid layered on top
kailali_dist <- c("KAILALI")
plot(st_geometry(np_dist), axes = TRUE, graticule = T)
plot(st_geometry(np_dist[np_dist$DISTRICT %in% kailali_dist, ]), 
     add = TRUE, col = sf.colors(8, categorical = T))
plot(sf::st_geometry(sf::st_centroid(np_dist[np_dist$DISTRICT %in% kailali_dist, ])), 
     pch = 4, cex = 2.2, col = "red", add = TRUE)

# ggplot plotting
kailali_dist <- c("KAILALI")
kailali_gg <- ggplot() +
  geom_sf(data = st_geometry(np_dist), fill = "white", lwd = 0.3) + 
  geom_sf(data = st_geometry(np_dist[np_dist$DISTRICT %in% kailali_dist, ]), 
          aes(fill = kailali_dist), lwd = 0.3) +
  geom_sf(data = st_geometry(sf::st_centroid(np_dist[np_dist$DISTRICT %in% kailali_dist, ]))) +
  ggrepel::geom_text_repel(data=(np_dist[np_dist$DISTRICT %in% kailali_dist, ]) %>% 
                              mutate(lon=map_dbl(.$geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
                                     lat=map_dbl(.$geometry, ~st_centroid(.x)[[2]])), 
                   aes(x=lon, y=lat, label=DISTRICT), size = 2) + # default size is big
  scale_y_continuous() +
  guides(fill=FALSE) + # remove legend
  xlab(NULL) + ylab(NULL) +
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "wheat2")) # change the grid (graticule) color

kailali_gg
ggsave("./outputs/selected_kailali.png", 
       plot = kailali_gg, units = "in", dpi = 300, width = 10)
