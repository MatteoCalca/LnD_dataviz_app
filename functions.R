library(countrycode)

# List of functions
f <- list()

f$rename_country <- function(data,country_column){
  data %>%
    mutate({{country_column}}:=countrycode(sourcevar={{country_column}},origin="iso3c",destination="country.name"))
}

f$rename_impacts <- function(data,impacts_column){
  data %>% mutate({{impacts_column}}:=case_when({{impacts_column}} == "burkesr" ~ "Empirical, national",
                                                {{impacts_column}} == "kalkuhl" ~ "Empirical, sub-national",
                                                {{impacts_column}} == "coacch" ~ "Process-based"))
}

f$rename_responsibility <- function(data,resp_column){
  data %>% mutate({{resp_column}}:=case_when({{resp_column}} == "share15" ~ "from 2015",
                                             {{resp_column}} == "share50" ~ "from 1850",
                                             {{resp_column}} == "share90" ~ "from 1990"),
                  {{resp_column}} := factor({{resp_column}},levels=c("from 1850","from 1990","from 2015"),ordered = T))
}

f$clean_data <- function(data){
  data %>% 
    select(-X,-t,-POL) %>% 
    f$rename_country(country_column = n) %>% 
    f$rename_impacts(impacts_column = IMP)
}