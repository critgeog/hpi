# HPI price appreciation

library(tidyverse)
library(tidycensus)
library(sf)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
#library(tmap)
#library(RColorBrewer)
library(purrr)
library(magrittr)

hpi <- read_csv("../../HPI_AT_BDL_tract.csv",
                   col_types = cols(
                     tract = col_character()
                     )
                   )
hpi <- hpi %>%
  select(tract, state_abbr, year, hpi) %>%
  spread(year,hpi)

# this is all it takes to get a tibble of total population estimates 
# for all US Census tracts from the 2011-2015 ACS: https://walkerke.github.io/2017/05/tidycensus-every-tract/

us <- unique(fips_codes$state)[1:51]

# totalpop <- map_df(us, function(x) {
#   get_acs(geography = "tract", variables = "B01003_001", 
#           state = x)
# })
# 
# str(totalpop)

# However - what if you also want tract geometry for mapping? This only requires a few small modifications. map_df in purrr uses the bind_rows function under the hood, which doesn’t work with simple features objects (yet). However, sf does have an rbind method that works for sf objects and can be fed to purrr’s reduce function.
totalpop_sf <- reduce(
  map(us, function(x) {
    get_acs(geography = "tract", variables = "B01003_001", 
            state = x, geometry = TRUE)
  }), 
  rbind
)

# join hpi data to census data
hpi_sf <- left_join(totalpop_sf, hpi, by = c('GEOID' ='tract'))  

hpi_0217 <- hpi_sf %>%
  select(GEOID, estimate, `2002`:`2017`) %>%
  mutate_at(3:18, as.double)

# write_sf(hpi_0217, "hpi_0217.shp")
write_csv(hpi_0217, "hpi_0217.csv")

# reduce dataset to Atlanta metro
# atl <- core_based_statistical_areas(cb = TRUE) %>%
#   filter(GEOID %in% c("12060")) %>%
#   select(metro_name = NAME)
# 
# atl_hpi_sf <- st_join(ga_hpi_sf, atl, join = st_within, 
#                    left = FALSE)


# library(leaflet)
# pal = colorBin("Greens", domain = atlhpi98$hpi, bins = 5)

# leaflet(data = hpi_0217) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>% 
#   addPolygons(colors = 'Greens', stroke = FALSE) %>% 
#   addLegend(colors = 'Greens', values = ~2017) %>% 
#   setView(lng = -85.4, 40, zoom = 12) %>% 
#   addMiniMap()

# hpi_all_0217 <- hpi_all_0217 %>%
#   st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
#                +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 
#                +units=m +no_defs")

# hpi_all_0217$`2017` <- as.double(hpi_all_0217$`2017`)




