library(fitzRoy)
library(tidyverse)
results <- get_match_results()
tail(results$Season)

results_2018_long <- 
  results %>%
    filter(Season == 2018) %>%
    convert_results()

head(results_2018_long)
results_2018_long %>%
  filter(Team == "Collingwood") %>%
  View()
