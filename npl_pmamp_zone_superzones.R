require(sf)
require(tidyverse)
require(ggpmisc)

# read nepal district map
np_dist <- sf::st_read("./data/npl_districts/NPL_districts_poly_sd_171123.shp") %>% 
  mutate(DISTRICT = fct_recode(DISTRICT, "CHITWAN" = "CHITAWAN"))

## Plotting districtwise pmamp production profile
dist_wise_pmamp <- readxl::read_xlsx("./data/npl_pmamp_superzone_zone_block_pockets.xlsx", na = " ", sheet = "zone_superzone", skip = 1)

dist_wise_pmamp <- dist_wise_pmamp %>% 
  mutate(District = str_to_upper(District)) %>% 
  mutate(District = str_replace_all(District,
                                    pattern = c("KAVREPALANCHOK" = "KABHREPALANCHOK", 
                                                "MAKWANPUR" = "MAKAWANPUR", 
                                                "KAPILVASTU" = "KAPILBASTU"))) %>% 
  rename(DISTRICT = District)

np_dist$DISTRICT[which(!np_dist$DISTRICT %in% dist_wise_pmamp$DISTRICT)]

dist_wise_pmamp <- dist_wise_pmamp %>% 
  select(-SN) %>% 
  group_by(DISTRICT
           # , `Zone/Superzone`
           ) %>% 
  summarise_all(list(~paste(., collapse = ", "))) %>% 
  mutate(`Zone/Superzone` = case_when(
    `Zone/Superzone` == "Zone, Zone, Zone" ~ "Zone",
    `Zone/Superzone` == "Zone, Zone" ~ "Zone",
    `Zone/Superzone` == "Zone, Zone, Superzone" ~ "Superzone, Zone",
    `Zone/Superzone` == "Superzone, Zone, Zone" ~ "Superzone, Zone",
    TRUE ~ `Zone/Superzone`
    ))

np_dist <- full_join(np_dist, dist_wise_pmamp, by = "DISTRICT")

pmamp_profile <- ggplot() +
  geom_sf(data = st_geometry(np_dist), aes(fill = np_dist$`Zone/Superzone`), lwd = 0.3, color = "black") + 
  ggrepel::geom_label_repel(data = (np_dist %>% filter(!is.na(`Crop`))) %>% 
                              mutate(lon=map_dbl(.$geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
                                     lat=map_dbl(.$geometry, ~st_centroid(.x)[[2]])), 
                            aes(x=lon, y=lat, label=Crop), size = 2) +
  # best way to determine the position is through st_bbox() to get the bounding box
  annotate(geom = "table", x = 665000, y = 3550000, label = list(np_dist %>% 
                                                                   mutate(DISTRICT = str_to_title(DISTRICT)) %>% 
                                                                   st_set_geometry(NULL) %>%
                                                                   filter(!is.na(`Crop`)) %>%
                                                                   select(District = DISTRICT, Crop) %>% 
                                                                   split(f = as.factor(rep(1:4, each = 16))) %>% 
                                                                   bind_cols() %>% 
                                                                   magrittr::set_colnames(rep(c("District", "Commodity"), times = 4))
  ),
           vjust = 1, hjust = 0, size = 1.8) +
  scale_y_continuous() +
  xlim(120000, 1400000) + 
  # guides(fill=FALSE) + # remove legend
  xlab(NULL) + ylab(NULL) +
  labs(fill = "PMAMP program") +
  theme_bw() + 
  ggtitle("District profile of pmamp program and respective commodities on FA 2017/18\n(Source: pmamp.gov.np, 2019)") +
  theme(panel.grid.major = element_line(colour = "turquoise"), legend.position = "bottom", 
        legend.text = element_text(angle = 45), legend.text.align = +0.5) # change the grid (graticule) color

# st_crs(np_dist)

# ggsave behaving inconsistently, if once printed to graphics device save to external format has disturbed dimensions
# but once the graphics device is cleared and then ggsave() is run, then the dimensions are as expected.

# # do not run the graphics print because of ggsave inconsistencies
# pmamp_profile

ggsave("./outputs/npl_pmamp_profile.png", 
       plot = pmamp_profile, units = "in", dpi = 300, width = 14, height = 8)
