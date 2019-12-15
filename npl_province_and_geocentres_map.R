require(sf)
require(tidyverse)

require(units)

# read nepal district map
np_dist <- sf::st_read("./data/npl_districts/NPL_districts_poly_sd_171123.shp")
np_province <- sf::st_read("./data/npl_province/Nepal_Province.shp")

# transform crs of district map to EPSG "4326" (same as that of province map)
np_dist <- st_transform(np_dist, crs = "+proj=longlat +datum=WGS84 +no_defs")

# geometry of provinces
plot(st_geometry(np_province))
seven_province <- np_province$State_Name

# ggplot2 plotting
eight_gg <- ggplot() +
  geom_sf(data = st_geometry(np_dist), fill = "white", lwd = 0.3) + 
  geom_sf(data = st_geometry(np_province), aes(fill = seven_province), 
          alpha = 0.5, lwd = 0.5, color = "blue") + 
  geom_sf(data = st_geometry(sf::st_centroid(np_province))) +
  ggrepel::geom_label_repel(data=(np_province %>% 
                             mutate(lon=map_dbl(.$geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
                                    lat=map_dbl(.$geometry, ~st_centroid(.x)[[2]]))), 
                           aes(x=lon, y=lat, label=State_Name), size = 2) + # default size is big
  scale_y_continuous() +
  guides(fill=FALSE) + # remove legend
  xlab(NULL) + ylab(NULL) +
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "wheat2")) # change the grid (graticule) color
eight_gg

ggsave("./outputs/nepal_province_and_geocentres_map.png", 
       plot = eight_gg, units = "in", dpi = 300, width = 10)
