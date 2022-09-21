library(dplyr)
library(tidyverse)

rm(list=ls())
#Pull in Advanced Data
year = 2011
gamedataadvanced <- data.frame()

while (year <= 2022) {
  if (year == 2020) {
    year = year+1
  } else {
    str1 = as.character(year)
    str2 = "_super_sked.csv"
    dataread <- paste(str1,str2, sep = "")
    data <- read_csv(dataread,col_names = FALSE) 
    gamedataadvanced <- rbind(gamedataadvanced, data)
    year = year+1
  }
}

names(gamedataadvanced) <- c('muid', 'date', 'conmatch', 'matchup', 'prediction', 'ttq', 'conf', 'venue', 
'team1', 't1oe', 't1de', 't1py', 't1wp', 't1propt', 'team2', 't2oe', 't2de', 't2py', 't2wp', 
't2propt', 'tpro', 't1qual', 't2qual', 'gp', 'result', 'tempo', 'possessions', 't1pts', 
't2pts', 'winner', 'loser', 't1adjt', 't2adjt', 't1adjo', 't1adjd', 't2adjo', 't2adjd',
'gamevalue', 'mismatch', 'blowout', 't1elite', 't2elite', 'ord_date', 't1ppp', 't2ppp', 'gameppp',
't1rk', 't2rk', 't1gs', 't2gs', 'gamestats', 'overtimes', 't1fun', 't2fun', 'results')

#REF TABLES
team_ref <- read_csv("MTeams.csv")
city_ref <- read_csv("Cities.csv")
conf_ref <- read_csv("Conferences.csv")
region_ref <- read_csv("MSeasons.csv")
seed_ref <- read_csv("MNCAATourneySeeds.csv")

#Game Data
city_results <- read_csv("MGameCities.csv")
tourney_box_results <- read_csv("MNCAATourneyDetailedResults.csv")
regular_box_results <- read_csv("MRegularSeasonDetailedResults.csv")