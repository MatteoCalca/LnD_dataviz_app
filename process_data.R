library(giscoR)
library(sf)
library(tidyverse)

source("functions.R")

color_palette <- list()
color_palette$map <- c(low="#1446A0",mid = "white",high="#DB3069")

world_map<- giscoR::gisco_get_countries(resolution = "60") %>% 
  filter(ISO3_CODE != "ATA") %>% 
  select(n=ISO3_CODE, geometry) %>% 
  f$rename_country(country_column = n)

damages_by_country <-
  read.csv("data/damages_by_country.csv") %>%
  filter(n!="row") %>% 
  f$clean_data() 
  
damages_by_country <-
  bind_rows(damages_by_country,
            damages_by_country %>% 
              group_by(n,gnipc,gni,income) %>% 
              summarise(damn = mean(damn)) %>% 
              mutate(IMP = "Mean across functions")) %>% 
  mutate(damages_percGDP = damn/(gni*100), #tnUSD / tnUSD
         damn = damn * 1000) #bnUSD

compensation_by_country <-
  read.csv("data/Compensation_by_country.csv") %>%
  filter(n!="row") %>% 
  f$clean_data() %>% 
  left_join(damages_by_country %>% select(n,IMP,gni))
  
compensation_by_country <-
  bind_rows(compensation_by_country,
            compensation_by_country %>% 
              group_by(n,resp,income,gni) %>% 
              summarise(comp = mean(comp)) %>% 
              mutate(IMP = "Mean across functions")) %>% 
  mutate(compensation_percGDP = (comp/1000) / (gni*100)) #tnUSD / tnUSD

map_damages <- left_join(world_map,damages_by_country)

map_compensation <- left_join(world_map,compensation_by_country)

# map_damages %>%
#   filter(IMP %in% c(unique(map_damages$IMP)[3],NA)) %>%
# ggplot(aes(fill=damages_percGDP))+
#   geom_sf() +
#   scale_fill_gradient2(low="#1446A0",
#                          mid = "white",
#                          high="#DB3069",
#                          midpoint=0,
#                          na.value = "gray80",
#                          label = scales::label_percent(),
#                          limits = c(min(map_damages$damages_percGDP,na.rm = T),max(map_damages$damages_percGDP,na.rm = T)))






# compensation_by_country %>%
#   mutate(comp_sign = sign(comp)) %>%
#   group_by(IMP,resp,comp_sign) %>%
#   summarize(comp = sum(comp)) %>%
#   pivot_wider(names_from = resp,values_from = comp)
# 
# damages_by_country %>% 
#   mutate(damn_sign = sign(damn)) %>% 
#   group_by(IMP,damn_sign) %>% 
#   summarize(damn = sum(damn), gni = sum(gni))
  