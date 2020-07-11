library(rnaturalearth)

ukraine <- ne_states(country='ukraine', returnclass = 'sf')
crimea <- ne_states(country='russia', returnclass = 'sf') %>% filter(name == 'Crimea')
ukraine <- rbind(ukraine, crimea) %>% st_union(by_feature = FALSE)
rm(crimea)

water <- ne_download(scale='large', type='lakes', 
                     category='physical', returnclass = 'sf') %>% 
  st_intersection(ukraine)

dnipro <- ne_download(scale='large', type='rivers_lake_centerlines', 
                      category='physical', returnclass = 'sf') %>% 
  filter(name_en == "Dnieper") %>% 
  st_intersection(ukraine)
