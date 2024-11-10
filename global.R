library(shiny)
library(ggplot2)
library(giscoR)
library(sf)
library(tidyverse)
library(plotly)
library(scales)
library(bslib)

source("functions.R")

color_palette <- list()
color_palette$map <- c(low="#1446A0",mid = "white",high="#DB3069")
color_palette$bars <- c(low="#E5B181",mid = "#F1F2F6",high="#81B29A")


world_map<- giscoR::gisco_get_countries(resolution = "60") %>% 
  filter(ISO3_CODE != "ATA") %>% 
  select(n=ISO3_CODE, geometry) %>% 
  f$rename_country(country_column = n)

damages_by_country <- read.csv("damages_by_country.csv") %>%
  filter(n!="row") %>% 
  f$clean_data() 
  
damages_by_country <- bind_rows(damages_by_country,
            damages_by_country %>% 
              group_by(n,gnipc,gni,income) %>% 
              summarise(damn = mean(damn)) %>% 
              mutate(IMP = "Mean across functions")) %>% 
  mutate(damages_percGDP = damn/(gni*100), #tnUSD / tnUSD
         damn = damn * 1000,
         IMP = factor(IMP,levels = c("Mean across functions","Empirical, national","Empirical, sub-national","Process-based"),ordered=T)) #bnUSD

compensation_by_country <- read.csv("compensation_by_country.csv") %>%
  filter(n!="row") %>% 
  f$clean_data() %>% 
  f$rename_responsibility(resp_column = resp) %>% 
  left_join(damages_by_country %>% select(n,IMP,gni)) %>%  # Join national GDP
  filter(n!="Venezuela")
  
compensation_by_country <- bind_rows(compensation_by_country,
            compensation_by_country %>% 
              group_by(n,resp,income,gni) %>% 
              summarise(comp = mean(comp)) %>% 
              mutate(IMP = "Mean across functions")) %>% 
  mutate(compensation_percGDP = (comp/1000) / (gni*100)) #tnUSD / tnUSD

map_damages <- left_join(world_map,damages_by_country)

#map_compensation <- left_join(world_map,compensation_by_country)
# 
# compensation_binned <- compensation_by_country %>%
#      filter(IMP %in% c(unique(map_damages$IMP)[3],NA),
#             resp == unique(map_compensation$resp)[2]) %>%
#   mutate(to_be_binned = if_else(abs(comp) < 0.01 * sum(abs(comp)),
#                                 if_else(sign(comp) == 1,"binned_positive","binned_negative"),
#                                 "not_binned"),
#          n = case_when(to_be_binned == "not_binned" ~ n,
#                        to_be_binned == "binned_positive" ~ "Other recipients",
#                        to_be_binned == "binned_negative" ~ "Other donors")) %>%
#   group_by(n) %>%
#   summarise(comp = sum(comp), gni = sum(gni,na.rm = T)) %>%
#   mutate(compensation_percGDP = (comp/1000) / (gni*100)) #tnUSD / tnUSD
# #
#   compensation_binned %>%
#     mutate(dummy="dummy") %>%
#   ggplot(aes(x=reorder(n,comp),y=comp,fill=comp,text=n))+
#   geom_col(position = "dodge",color="black",linewidth=0.01) +
#   geom_vline(xintercept = 0) +
#   theme_void() +
#     scale_fill_gradient2(low=color_palette$bars[["low"]], mid = color_palette$bars[["mid"]], high=color_palette$bars[["high"]], midpoint=0, na.value = "gray80")+
#   theme(axis.line.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title = element_blank(),
#         axis.text.x = element_text(color = "gray20",angle = 45),
#         axis.text.y = element_text(color = "gray20"),
#         panel.grid.major.x  = element_line(color = "gray90"),
#         legend.title = element_blank()) #-> p

# 
# ggplotly(p)


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
  