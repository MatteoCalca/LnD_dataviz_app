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
         damn = damn * 1000, #bnUSD
         IMP = factor(IMP,levels = c("Mean across functions","Empirical, national","Empirical, sub-national","Process-based"),ordered=T)) 

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

  