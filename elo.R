library(elo)
library(fitzRoy)
library(lubridate)
library(tidyverse)

# source : https://analysisofafl.netlify.com/fitzroy/2018-07-23-build-a-quick-elo/

results <- fitzRoy::get_match_results()
results <- results %>%filter(Season %in% c(2017))%>%
  mutate(seas_rnd = paste0(Season, ".", Round.Number),
         First.Game = ifelse(Round.Number == 1, TRUE, FALSE)
  )


# Simple ELO
# Set parameters
HGA <- 0
carryOver <- 1
B <- 0.03
k_val <- 20

# Create margin function to ensure result is between 0 and 1
map_margin_to_outcome <- function(margin, B) {
  1 / (1 + (exp(-B * margin)))
}

# Run ELO
elo.data <- elo.run(
  map_margin_to_outcome(Home.Points - Away.Points, B = B) ~
    adjust(Home.Team, HGA) +
    Away.Team +
    group(seas_rnd) ,
  # regress(First.Game, 1500, carryOver),
  k = k_val,
  data = results
)

elo.data$initial.elos
# as.data.frame(elo.data)
# as.matrix(elo.data)
# final.elos(elo.data)


# Do predictions
fixture <- fixture %>%filter(Season==2018)%>%
  mutate(Prob = predict(elo.data, newdata = fixture))

results <- fitzRoy::get_match_results()
# View(fixture)

results<-select(results,Date, Home.Team, Margin)

left_join(fixture, results, by=c("Date"="Date","Home.Team"="Home.Team" ))%>%
  mutate(pick=if_else(Prob>0.5,1,-1))%>%
  mutate(correct=pick*Margin)%>%
  mutate(right_pick=if_else(correct>0,1,0))%>%
  mutate(accuracy=sum(right_pick, na.rm = TRUE))%>%View()