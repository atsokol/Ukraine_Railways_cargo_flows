
# Summarise trends in data
data %>% 
  filter(date %within% interval(ymd(20190401), ymd(20190531)) | 
         date %within% interval(ymd(20200401), ymd(20200531)) ) %>% 
  mutate(year = year(date) %>% as.factor()) %>% 
  group_by(cargo, year) %>% 
  summarise(vah = sum(vahoniv)) %>% 
  ggplot(aes(x=year, y=vah)) +
  geom_bar(stat='identity') +
  facet_wrap(~cargo)
