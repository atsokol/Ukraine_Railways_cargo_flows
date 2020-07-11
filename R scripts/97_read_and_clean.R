library(jsonlite)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(tsibble)
library(dplyr)
library(data.table)

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

# rm(pp, urls, pass)

# Load the shapefiles
stations <- st_read("./data/Shapefiles/Ukraine_railway_stations.shp") %>% 
  select(fid, name) %>% 
  mutate(name = toupper(name))
ukraine <- st_read("./data/Shapefiles/Ukraine_with_Crimea.gpkg")
rail_network_ne <- st_read("./data/Shapefiles/ne_10m_railroads.shp") %>% st_crop(ukraine) %>% 
  filter(st_geometry_type(geometry) == 'LINESTRING') %>% select(rwdb_rr_id, scalerank)

# Match station names to codes using fuzzy string matching and filter out NAs
data$origin <- str_remove(data$origin, "(\\(.*\\))|(-ЕКСП\\.)|(-ЕКСПОРТ)|(\\s)+$")
data$destination <- str_remove(data$destination, "(\\(.*\\))|(-ЕКСП\\.)|(-ЕКСПОРТ)|(\\s)+$")
data_clean <- data %>% 
  mutate(org = amatch(data$origin, stations$name, method='jw', p=0.1)) %>% 
  mutate(dst = amatch(data$destination, stations$name, method='jw', p=0.1)) %>%
  filter(!is.na(org) & !is.na(dst)) %>% 
  filter(dst != org) %>% select(org, dst, everything()) 

# Check percentage of traffic captured
data %>% group_by(date) %>% summarise(all = sum(vahoniv)) %>% 
  left_join(data_clean %>% group_by(date) %>% summarise(clean = sum(vahoniv))) %>% 
  mutate(perc = clean / all)



