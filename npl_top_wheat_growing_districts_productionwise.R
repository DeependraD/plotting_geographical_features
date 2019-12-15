require(sf)
require(tidyverse)
require(ggpmisc)

# read nepal district map
np_dist <- sf::st_read("./data/npl_districts/NPL_districts_poly_sd_171123.shp") %>% 
  mutate(DISTRICT = fct_recode(DISTRICT, "CHITWAN" = "CHITAWAN"))

## Plotting districtwise wheat production profile
dist_wise_wheat <- readxl::read_xlsx("./data/npl_wheat_districts_0506-1516.xlsx", sheet = "production", skip = 1)

dist_wise_wheat <- dist_wise_wheat %>% 
  mutate(District = str_to_upper(District)) %>% 
  mutate(District = str_replace_all(District,
                                    pattern = c("KAVREPALANCHOK" = "KABHREPALANCHOK", 
                                                "MAKWANPUR" = "MAKAWANPUR", 
                                                "KAPILVASTU" = "KAPILBASTU"))) %>% 
  rename(DISTRICT = District)

np_dist <- left_join(np_dist, dist_wise_wheat, by = "DISTRICT") %>% 
  rename(Year201516 = `2015/16`)

wheat_production_1516 <- ggplot() +
  geom_sf(data = st_geometry(np_dist), aes(fill = np_dist$Year201516), lwd = 0.3, color = "black") + 
  ggrepel::geom_label_repel(data = (np_dist %>% top_n(10, wt = Year201516) %>% arrange(desc(Year201516))) %>% 
                              mutate(lon=map_dbl(.$geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
                                     lat=map_dbl(.$geometry, ~st_centroid(.x)[[2]])), 
                            aes(x=lon, y=lat, label=DISTRICT), size = 2) +
  # best way to determine the position is through st_bbox() to get the bounding box
  annotate(geom = "table", x = 690000, y = 3380000, label = list(np_dist %>%
                                                            st_set_geometry(NULL) %>%
                                                            top_n(10, wt = Year201516) %>%
                                                            arrange(desc(Year201516)) %>%
                                                            select(DISTRICT, "Production (Ton)"= Year201516)),
           vjust = 1, hjust = 0) +
  scale_y_continuous() +
  scale_fill_gradient(name = "Production in Tons", guide = "colourbar", low = "#132BCC", high = "#CC2B20") +
  # guides(fill=FALSE) + # remove legend
  xlab(NULL) + ylab(NULL) +
  theme_bw() + 
  ggtitle("Estimated districtwise production of wheat grain in Nepal, featuring top 10 districts during\nYear: 2015/16\n(Source: Statistical Year Book, 2017)") +
  theme(panel.grid.major = element_line(colour = "turquoise"), legend.position = "bottom", 
        legend.text = element_text(angle = 45), legend.text.align = +1) # change the grid (graticule) color

# st_crs(np_dist)

# ggsave behaving inconsistently, if once printed to graphics device save to external format has disturbed dimensions
# but once the graphics device is cleared and then ggsave() is run, then the dimensions are as expected.

# # do not run the graphics print because of ggsave inconsistencies
# wheat_production_1516

ggsave("./outputs/npl_top_wheat_growing_districts_productionwise.png", 
       plot = wheat_production_1516, units = "in", dpi = 300, width = 12, height = 8)
