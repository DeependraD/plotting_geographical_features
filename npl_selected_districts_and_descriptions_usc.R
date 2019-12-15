# requirements
require(sf)
require(tidyverse)
require(units)

# read nepal district map
np_districts <- sf::st_read("./data/npl_districts/NPL_districts_poly_sd_171123.shp")
np_districts_production <- sf::st_read("./data/npl_selected_districts_and_descriptions_usc/seed_producing_districts_USC.shp")
np_districts_prod_transact <- tribble(~"district", ~"seed_production", 
                                      "Darchula", "Yes",
                                      "Baitadi", "Yes", 
                                      "Dadeldhura", "Yes",
                                      "Kanchanpur", "Yes", 
                                      "Doti", "Yes",
                                      "Kailali", "Yes",
                                      "Achham", "Yes"
                                      )

# geometry of provinces
plot(st_geometry(np_districts_production))
plot(st_geometry(np_districts))
np_districts_production$DISTRICT

## mutate production-transaction record sheet to match district names;
# will uppercasing only do the work?
np_districts$DISTRICT %in% (np_districts_prod_transact %>% 
  mutate(District = str_to_upper(district)) %>% 
  pull(District)) %>% which() %>% length()

# so, yes only uppercasing will do the job
np_districts_prod_transact <- np_districts_prod_transact %>% 
  mutate(District = str_to_upper(district))

# ggplot2 plotting
np_districts <- np_districts %>% 
  left_join(np_districts_prod_transact, by = c("DISTRICT" = "District"))

eight_gg <- ggplot() +
  geom_sf(data = st_geometry(np_districts), aes(fill = np_districts$Distribution_record), lwd = 0.3) + 
  # geom_sf(data = st_geometry(np_districts_production), 
  #         alpha = 0.2, lwd = 0.5, color = "blue", aes(fill = "wheat3")) +
  scale_fill_manual(values = c("wheat3", "green"), labels = c("No", "Yes")) +
  labs(fill = "Have transaction?", 
       title = "Districts where production and sales have taken place", 
       subtitle = "* Districts with text labels are the producer districts") +
  ggrepel::geom_label_repel(data=(np_districts_production %>% 
                             mutate(lon=map_dbl(.$geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
                                    lat=map_dbl(.$geometry, ~st_centroid(.x)[[2]]))), 
                           aes(x=lon, y=lat, label=DISTRICT), size = 2) + # default size is big
  scale_y_continuous() +
  # guides(fill=FALSE) + # remove legend
  xlab(NULL) + ylab(NULL) +
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "wheat2"), 
        plot.subtitle = element_text(hjust = 1.0, size = 8)) # change the grid (graticule) color
eight_gg

ggsave("./outputs/npl_selected_districts_and_descriptions_usc.png", 
       plot = eight_gg, units = "in", dpi = 300, width = 10)
