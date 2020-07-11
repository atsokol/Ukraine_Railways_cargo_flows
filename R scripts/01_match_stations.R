library(jsonlite)
library(tidyr)
library(lubridate)
library(stringr)
library(dplyr)
library(data.table)
library(osmdata)
library(sf)
library(tmap)
library(stringdist)
library(fuzzyjoin)
library(rnaturalearth)
library(ggmap)

# Download data passport from source
pp <- 'https://data.gov.ua/api/3/action/package_show?id=e5dc8431-265e-402a-8912-aaad632aa827'
pass <- read_json(pp, simplifyVector = TRUE)
urls <- pass$result$resources$url

read_and_clean <- function(url){
  temp <- tempfile()
  download.file(url, temp)
  frame <- readxl::read_excel(temp, 
                              col_types = c("text", "text", "text", "numeric",
                                            "numeric", "numeric", "numeric"),
                              col_names = c("cargo", "origin", "destination", "vahoniv",
                                            "ton", "ton_km", "payment"),
                              skip = 1)
  frame <- frame %>% filter(cargo!="Итого" | origin!="Итого" | destination!="Итого") %>% 
    fill(c("cargo", "origin"))
  frame$date <- str_extract(url, "\\d{2}_\\d{2}_\\d{4}") %>% 
    as.Date(format='%d_%m_%Y') %>%
    floor_date(unit='month')
  return(frame)
}

data_list <-  map(urls, read_and_clean)
data <- rbindlist(data_list) %>% as_tibble() %>% drop_na()

# Correct manual errors 
data[data$date == '2017-06-01', ]$date <-  as.Date('2019-06-01')
data$cargo <-  str_trim(data$cargo)

rm(data_list, pass, pp, urls)

#  Downloan OSM data on railway stations
region <- ne_download(scale=110, type='countries', category='cultural', returnclass = 'sf') %>% 
  filter(ADMIN %in% c('Russia', 'Estonia', 'Latvia', 'Lithuania',
                      'Slovakia', 'Hungary', 'Romania', 'Ukraine', 'Moldova', 
                      'Belarus', 'Kazakhstan', 'Poland', 'Georgia', 'Azerbaijan',
                      'Armenia', 'Uzbekistan', 'Turkmenistan', "Tajikistan", 'Kyrgyzstan',
                      'Bulgaria')) 

set_overpass_url('https://lz4.overpass-api.de/api/interpreter') 

osm_df <- opq(bbox = st_bbox(region), timeout = 2000) %>% 
  add_osm_feature(key = 'railway', value = 'station') %>% 
  osmdata_sf()

stations_osm <- osm_df$osm_points %>% 
  select(osm_id, name) %>% 
  mutate(name = toupper(name)) %>% 
  drop_na()

rm(region, osm_df)

## Match stations 
stations <- c(data$origin, data$destination) %>% unique() %>% as_tibble()

# Match station names to OSM database
batch1 <- stations %>%
  mutate(value_c = str_remove(str_trim(value), "\\(.+\\)")) %>% 
  stringdist_inner_join(stations_osm %>% st_drop_geometry() %>% as_tibble() %>% 
                         mutate(name = str_trim(name)), 
                       by = c(value_c = "name"), max_dist=0) %>% 
  left_join(stations_osm) %>% 
  select(value, geometry) %>% 
  st_as_sf()

# Find unmatched stations and geocode them through Google
register_google('AIzaSyCxRr5X230ATq_h8rQAUBzEnNgmkRZi9a4')

unmatched <- stations %>%
  mutate(value_c = str_remove(str_trim(value), "\\(.+\\)")) %>% 
  stringdist_anti_join(stations_osm %>% st_drop_geometry() %>% as_tibble() %>% 
                         mutate(name = str_trim(name)), 
                       by = c(value_c = "name"), max_dist=0) %>% 
  mutate(value_c = paste0(value_c, " станция")) %>% 
  mutate_geocode(value_c) 

batch2 <- unmatched %>% 
  drop_na() %>% 
  st_as_sf(coords=c('lon','lat'))

st_crs(batch2) = 4326

# Repeat geocoding without word 'station'
unmatched_2 <- unmatched %>% 
  filter(is.na(lon)) %>% 
  select(value) %>% 
  mutate(value_c = str_remove(str_trim(value), "\\(.+\\)")) %>% 
  mutate_geocode(value_c) 

batch3 <- unmatched_2 %>% 
  drop_na() %>% 
  st_as_sf(coords=c('lon','lat')) %>% 
  select(value)

st_crs(batch3) = 4326

# Match remaining stations by hand 
# hand_match <- unmatched_2 %>% 
#   filter(is.na(lon)) %>% 
#   select(value) %>% 
#   arrange(value)

hand_match <- readxl::read_xlsx('./data/hand_match.xlsx') %>% 
  drop_na() %>% 
  st_as_sf(coords=c('lon','lat')) %>% 
  rename(name='value')

st_crs(hand_match) = 4326

# Combine all results and remove duplicates
stations_sf <- rbind(batch1, batch2, batch3) %>% 
  filter(!duplicated(stations_sf %>% st_drop_geometry()))

stations_sf %>% 
  filter(!duplicated(stations_sf %>% st_drop_geometry())) %>% 
  rename(name = 'value')

rm(batch1, batch2, batch3, stations, unmatched, unmatched_2, hand_match)

# Check percentage of traffic captured
data %>% 
  filter(!(origin %in% stations_sf$name) | !(destination %in% stations_sf$name)) %>% 
  group_by(destination) %>% 
  summarise(tot = sum(vahoniv)) %>% 
  arrange(desc(tot))

  
