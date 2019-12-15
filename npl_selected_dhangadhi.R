require(sf)
require(tidyverse)

# read nepal district map
np_dist <- sf::st_read("./data/npl_admin2_poly_sd_171123/NPL_Adm2_poly_sd_171123.shp")
kailali_dist <- np_dist[np_dist$DISTRICT == "KAILALI", ]

# ggplot plotting
dhangadhi_gg <- ggplot() +
  geom_sf(data = st_geometry(kailali_dist), fill = "white", lwd = 0.3) + 
  geom_sf(data = st_geometry(kailali_dist[kailali_dist$LU_Name %in% "Dhangadhi", ]), 
          aes(fill = "Dhangadhi"), lwd = 0.3) +
  ggrepel::geom_text_repel(data=(kailali_dist[kailali_dist$LU_Name %in% "Dhangadhi", ]) %>% 
                              mutate(lon=map_dbl(.$geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
                                     lat=map_dbl(.$geometry, ~st_centroid(.x)[[2]])), 
                   aes(x=lon, y=lat, label=LU_Name), size = 2) + # default size is big
  scale_y_continuous() +
  guides(fill=FALSE) + # remove legend
  xlab(NULL) + ylab(NULL) +
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "wheat2")) # change the grid (graticule) color

dhangadhi_gg
ggsave("./outputs/selected_dhangadhi.png", 
       plot = dhangadhi_gg, units = "in", dpi = 300, width = 10)
