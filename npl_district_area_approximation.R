require(sf)
require(tidyverse)

# read nepal district map
np_dist <- sf::st_read("./data/npl_districts/NPL_districts_poly_sd_171123.shp")
np_dist$DISTRICT
np_dist$DISTRICT <- np_dist$DISTRICT %>% fct_recode("CHITWAN" = "CHITAWAN")

# area of 6 districts including, kailali and kanchanpur
# where sarjoo-52 is cultivated
# total arable land is considered 0.5
# total sarjoo-52 grown land in cultivable land is considered 0.75
# multiplication factor of 100 gives measure in ha

cultivated_area <- (sum(sf::st_area(np_dist[np_dist$DISTRICT == "KAILALI",]), 
  sf::st_area(np_dist[np_dist$DISTRICT == "KANCHANPUR",]))/1000000)*3*0.5*0.75*100

measurements::conv_unit_options$area
measurements::conv_unit(units::drop_units(cultivated_area), 
                        from = "km2", "hectare")

# convert total district area of for JHAPA
measurements::conv_unit(units::drop_units((sf::st_area(np_dist[np_dist$DISTRICT == "JHAPA",])/1000000)), 
                        from = "km2", "hectare")

sf::st_area(np_dist[np_dist$DISTRICT == "JHAPA",])/10000 # should be equal to hectares
