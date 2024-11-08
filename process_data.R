damages_by_country <-
  read.csv("data/damages_by_country.csv") %>%
  f$clean_data() 

compensation_by_country <-
  read.csv("data/Compensation_by_country.csv") %>%
  f$clean_data()