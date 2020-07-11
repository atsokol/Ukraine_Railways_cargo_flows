# Construct an input data frame
data_in <- data_clean %>%
  filter(year(date) == 2019,
         cargo == 'руда заліз.і марг.') %>% 
  group_by(org, dst, origin, destination) %>% 
  summarise(vahoniv = sum(vahoniv)) %>% 
  ungroup()

# Construct network and nodes
net <- SpatialLinesNetwork(rail_network_ne) %>% 
  sln_clean_graph()

nodes <- sln2points(net) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

# Locate nodes on network closest to OD points
data_in <- data_in %>% 
  left_join(select(stations, fid, geometry), by=c("org"="fid")) %>% 
  left_join(select(stations, fid, geometry), by=c("dst"="fid")) %>%
  mutate(org = find_network_nodes(net, geometry.x %>% st_coordinates(), maxdist=50000),  
         dst = find_network_nodes(net, geometry.y %>% st_coordinates(), maxdist=50000)) %>% 
  select(-geometry.x, -geometry.y) %>% 
  filter(!is.na(org) & !is.na(dst)) %>% 
  filter(!org==dst)

# Compute OD lines and routes
lines <- od2line(flow = data_in, zones = nodes)

n <- 1:nrow(lines)
quiet_line2route <- purrr::quietly(line2route) # Suppress warning messages from line2route

route_list <- lapply(n, function(x){
  tryCatch(
    { r <- quiet_line2route(lines[x,], route_fun = route_local, sln=net)$result
      r$id <- x
      r  
    },
    error = function(e){ })
})

routes <- rbindlist(route_list) %>% st_as_sf() %>% 
  left_join(data_in %>% rowid_to_column(var = 'id'))

# Perform spatial aggregation
routes_agg <- routes %>% 
  group_by(date) %>% 
  nest() %>% 
  mutate(route = map(data, overline2, attrib='vahoniv')) %>% 
  select(route) %>% 
  unnest() %>% 
  st_as_sf()

# routes_agg <-  overline2(routes, attrib='vahoniv') 

tmap_mode('plot')
(g1 <- tm_shape(ukraine) +
    tm_polygons() +
    tm_shape(routes_agg) +
    tm_lines(col='blue', lwd='vahoniv', scale=5))
    + tm_facets(by = 'date', free.coords = FALSE))

# Some graphs
tmap_mode('view')
tm_shape(net@sl) + 
  tm_lines() +
  tm_shape(lines %>% st_cast("POINT")) + 
  tm_dots(col = 'red') +
  tm_shape(lines) +
  tm_lines(col = 'blue', alpha=0.3)

