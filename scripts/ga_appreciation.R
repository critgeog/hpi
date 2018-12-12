# ga_appreciation
# needs to be reviewed and edited. This is just saving old commands in appreciation.R

ga_hpi <- read_csv("../HPI_AT_BDL_GAtracts.csv",
                   col_types = cols(
                     tract = col_character()
                   )
)
hpi <- ga_hpi %>%
  select(tract, state_abbr, year, hpi) %>%
  spread(year,hpi)


#ga_hpi %>%
#  mutate(annual_change = as.double(annual_change))

ga_tidy <- get_acs(geography = "tract",
                   state = 'GA',
                   variables = c('B25001_001E',
                                 'B01003_001E'),
                   survey = "acs5",
                   geometry = TRUE)


#load_variables(year = '2016', dataset = "acs5") %>% View


# join hpi data to census data
ga_hpi_sf <- left_join(ga_tidy, ga_hpi, by = c('GEOID' ='tract'))  

# all GA data with missing 'annual_change'
ga_hpi_sf %>%
  filter(annual_change == '.') %>%
  select(GEOID, year, annual_change, hpi)

# all GA data with missing 'hpi'
ga_hpi_sf %>%
  filter(hpi == '.') %>%
  select(GEOID, year, annual_change, hpi)

# reduce dataset to Atlanta metro
atl <- core_based_statistical_areas(cb = TRUE) %>%
  filter(GEOID %in% c("12060")) %>%
  select(metro_name = NAME)

atl_hpi_sf <- st_join(ga_hpi_sf, atl, join = st_within, 
                      left = FALSE)

# all ATL data with missing 'annual_change'
atl_hpi_sf %>%
  filter(annual_change == '.') %>%
  select(GEOID, year, annual_change, hpi)

# all ATL data with missing 'hpi'
atl_hpi_sf %>%
  filter(hpi == '.') %>%
  select(GEOID, year, annual_change, hpi)

atl_hpi_sf$hpi <- as.double(atl_hpi_sf$hpi)
atl_hpi_sf$annual_change <- as.double(atl_hpi_sf$annual_change)
# atl_hpi_sf$year <- as.factor(atl_hpi_sf$year)

# atl_hpi_sf %>%
#   group_by(GEOID) %>%
#   filter(year == '2012') %>%
#   tm_shape(ga_hpi_sf) +
#   tm_fill('annual_change')

tm_shape(atl_hpi_sf) +
  tm_fill('annual_change') +
  tm_facets(by= 'year')

tm_shape(atl_hpi_sf) +
  tm_fill('hpi') +
  tm_facets(by= 'year')

filter(as.integer(Epsilon)>2)

atl_hpi_sf %>%
  filter(year > '1998') %>%
  tm_shape(atl_hpi_sf) +
  tm_fill('hpi') +
  tm_facets(by= 'year')





# cut down to after 1998
atl_hpi_sf %>%
  filter(year > '1998') -> atlhpi98

atlhpi98$year2 <- as.factor(atlhpi98$year)

tm_shape(atlhpi98) +
  tm_fill('hpi', style = 'jenks', palette = 'RdYlBu') +
  tm_facets(by= 'year2')

tm_shape(atlhpi98) +
  tm_fill('annual_change', breaks=c(-60,-30, 0, 30, 60, 80),
          palette = 'RdYlBu') +
  tm_facets(by= 'year2')

library(leaflet)
pal = colorBin("Greens", domain = atlhpi98$hpi, bins = 5)

?colorNumeric
?ColorBrewer

str(atlhpi98)
atlhpi98 %>%
  filter(year == 2008) -> atl08

leaflet(data = atl08) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(col = ~pal(hpi), stroke = FALSE) %>% 
  addLegend(pal = pal, values = ~hpi) %>% 
  setView(lng = -84.4, 33.8, zoom = 7) %>% 
  addMiniMap()

?addProviderTiles
#addCircles(col = ~pal(nbikes), opacity = 0.9) %>% 