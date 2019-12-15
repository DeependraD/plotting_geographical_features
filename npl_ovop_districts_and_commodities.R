require(sf)
require(tidyverse)
require(ggpmisc)

# read nepal district map
np_dist <- sf::st_read("./data/npl_districts/NPL_districts_poly_sd_171123.shp") %>% 
  mutate(DISTRICT = fct_recode(DISTRICT, "CHITWAN" = "CHITAWAN"))

## Plotting districtwise ovop production profile
dist_wise_ovop <- read_csv("./data/npl_ovop_districts_and_commodities_old.csv", na = " ", comment = "#")

ovop <- read_csv("./data/npl_ovop_districts_and_commodities_recent.csv")

ovop_splitted_df <- ovop %>% 
  mutate(District = str_replace_all(District, " ", "")) %>%
  mutate(District = str_split(District, ","))


dist_wise_ovop <- dist_wise_ovop %>% 
  mutate(District = str_to_upper(District)) %>% 
  mutate(District = str_replace_all(District,
                                    pattern = c("KAVREPALANCHOK" = "KABHREPALANCHOK", 
                                                "MAKWANPUR" = "MAKAWANPUR", 
                                                "KAPILVASTU" = "KAPILBASTU"))) %>% 
  rename(DISTRICT = District)

np_dist <- left_join(np_dist, dist_wise_ovop, by = "DISTRICT")

ovop_profile <- ggplot() +
  geom_sf(data = st_geometry(np_dist), aes(fill = np_dist$`OVOP program`), lwd = 0.3, color = "black") + 
  ggrepel::geom_label_repel(data = (np_dist %>% filter(`OVOP program`)) %>% 
                              mutate(lon=map_dbl(.$geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
                                     lat=map_dbl(.$geometry, ~st_centroid(.x)[[2]])), 
                            aes(x=lon, y=lat, label=DISTRICT), size = 2) +
  # best way to determine the position is through st_bbox() to get the bounding box
  annotate(geom = "table", x = 675000, y = 3450000, label = list(np_dist %>% 
                                                                   mutate(DISTRICT = str_to_title(DISTRICT)) %>% 
                                                                   st_set_geometry(NULL) %>%
                                                                   filter(`OVOP program`) %>%
                                                                   select(District = DISTRICT, Commodity) %>% 
                                                                   split(f = as.factor(rep(1:2, each = 16))) %>% 
                                                                   bind_cols() %>% 
                                                                   magrittr::set_colnames(rep(c("District", "Commodity"), times = 2))),
           vjust = 1, hjust = 0, size = 2) +
  scale_y_continuous() +
  # guides(fill=FALSE) + # remove legend
  xlab(NULL) + ylab(NULL) +
  labs(fill = "Has OVOP program?") +
  theme_bw() + 
  ggtitle("District profile of OVOP program in formulation and respective commodities on FA 2018/19\n(Source: AEC/FNCCI, 2019)") +
  theme(panel.grid.major = element_line(colour = "turquoise"), legend.position = "bottom", 
        legend.text = element_text(angle = 45), legend.text.align = +1) # change the grid (graticule) color

# st_crs(np_dist)

# ggsave behaving inconsistently, if once printed to graphics device save to external format has disturbed dimensions
# but once the graphics device is cleared and then ggsave() is run, then the dimensions are as expected.

# # do not run the graphics print because of ggsave inconsistencies
# ovop_profile

ggsave("./outputs/npl_ovop_profile.png", 
       plot = ovop_profile, units = "in", dpi = 300, width = 12, height = 8)
