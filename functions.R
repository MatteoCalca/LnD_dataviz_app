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

f$clean_data <- function(data){
  data %>% 
    select(-X,-t,-POL) %>% 
    f$rename_country(country_column = n) %>% 
    f$rename_impacts(impacts_column = IMP)
}