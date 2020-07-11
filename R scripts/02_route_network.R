library(sfnetworks)
library(sf)
library(tidygraph)
library(rnaturalearth)
library(tmap)
library(stplanr)
library(dplyr)
library(parallel)
library(lubridate)

# Clean data frame
data_clean <- data %>% 
  filter(origin %in% stations_sf$name, destination %in% stations_sf$name) 

# Build rail network graph
rail_network_ne <- st_read("./data/Shapefiles/ne_10m_railroads.shp") %>% 
  filter(st_geometry_type(geometry) == 'LINESTRING') %>% 
  filter(continent %in% c('Europe', 'Asia'))

net <- rail_network_ne %>% 
  SpatialLinesNetwork() %>% 
  sln_clean_graph()

nodes <- sln2points(net)

# Match stations to nearest nodes on graph
stations_sf <- stations_sf %>% 
  mutate(n_node = st_nearest_feature(geometry, nodes)) 

# Append node IDs to data frame
data_clean <- data_clean %>% 
  left_join(stations_sf %>% st_drop_geometry(), by=c('origin'='name')) %>%
  rename(org = 'n_node') %>%
  left_join(stations_sf %>% st_drop_geometry(), by=c('destination'='name')) %>%
  rename(dst = 'n_node') %>% 
  filter(org != dst)

# Unique routes
rts <- data_clean %>% 
  select(org, dst) %>% 
  unique() %>% 
  mutate(id = row_number())

# Calculate routes on a parallel cluster
cl <- makeCluster(detectCores())
clusterExport(cl, c("route_local", "net"))
routes <- route(from=nodes[rts$org,], to=nodes[rts$dst,], 
                route_fun=route_local, sln=net, cl=cl)
stopCluster(cl)

## Routes without parallel cluster
# routes <- route(from=nodes[rts$org,], to=nodes[rts$dst,], 
#                 route_fun=route_local, 
#                 sln=net)

routes <- routes %>% 
  select(route_number) %>% 
  left_join(rts, by=c('route_number' = 'id'))

# Aggregate routes
routes_agg <- data_clean %>% 
  mutate(year = year(date)) %>% 
  filter(year == '2019') %>% 
  group_by(org, dst) %>% 
  summarise(vahoniv = sum(vahoniv)) %>% 
  ungroup() %>% 
  left_join(routes, by = c('org'='org', 'dst'='dst')) %>% 
  st_as_sf() %>% 
  overline2(attrib='vahoniv')
  
  # group_by(date) %>% 
  # nest() %>% 
  # mutate(route = map(data, overline2, attrib='vahoniv')) %>% 
  # select(route) %>% 
  # unnest() %>% 
  # st_as_sf()

# Origin and destination points
point_org <- data_clean %>% 
  mutate(year = year(date)) %>% 
  filter(year == '2019') %>%
  group_by(origin) %>% 
  summarise(vahoniv = sum(vahoniv)) %>% 
  arrange(desc(vahoniv)) %>% 
  left_join(stations_sf, by=c('origin'='name')) %>% 
  st_as_sf()

point_dst <- data_clean %>% 
  mutate(year = year(date)) %>% 
  filter(year == '2019') %>%
  group_by(destination) %>% 
  summarise(vahoniv = sum(vahoniv)) %>% 
  arrange(desc(vahoniv)) %>% 
  left_join(stations_sf, by=c('destination'='name')) %>% 
  st_as_sf()

tmap_mode('plot')
(g2 <- tm_shape(ukraine) + tm_polygons() +
   # tm_shape(dnipro) + tm_lines(col = 'black', lwd=2.5) +
    tm_shape(dnipro) + tm_lines(col = 'white', lwd=2) +
    tm_shape(water) + tm_polygons(col = 'white', border.alpha = 0.3) +
    tm_shape(routes_agg) + tm_lines(col='blue', lwd='vahoniv', scale=5) +
    tm_shape(point_org) + tm_dots(size='vahoniv', col='red', alpha=0.5, shape=21, scale=2) +
    tm_shape(point_dst) + tm_dots(size='vahoniv', col='green', alpha=0.5, shape=21, scale=2))

tmap_save(g2, './pics/all_rail_routes.png')

tmap_mode('view')
(g3 <- tm_shape(routes_agg) + tm_lines(col='blue', lwd='vahoniv', scale=5) +
    tm_shape(point_org) + tm_dots(size='vahoniv', col='red', alpha=0.5, shape=21, scale=0.4) +
    tm_shape(point_dst) + tm_dots(size='vahoniv', col='green', alpha=0.5, shape=21, scale=0.4))
