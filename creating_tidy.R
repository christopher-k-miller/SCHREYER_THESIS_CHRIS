library(dplyr)
library(tidyverse)
library(data.table)
library(forecast)
library(kableExtra)
library(reshape2)
library(ggplot2)
library(scales)

rm(list=ls())

#This file can be ran all the way through, it will produce many data sets,
#the compare_results one gives overall games predicted for each method in each round

#This will run and put together the advanced data file
# #Pull in Advanced Data
# year = 2011
# gamedataadvanced <- data.frame()
# 
# while (year <= 2022) {
#   if (year == 2020) {
#     year = year+1
#   } else {
#     str1 = as.character(year)
#     str2 = "_super_sked.csv"
#     dataread <- paste(str1,str2, sep = "")
#     data <- read_csv(dataread,col_names = FALSE) 
#     gamedataadvanced <- rbind(gamedataadvanced, data)
#     year = year+1
#   }
# }
# 
# names(gamedataadvanced) <- c('muid', 'date', 'conmatch', 'matchup', 'prediction', 'ttq', 'conf', 'venue', 
# 'team1', 't1oe', 't1de', 't1py', 't1wp', 't1propt', 'team2', 't2oe', 't2de', 't2py', 't2wp', 
# 't2propt', 'tpro', 't1qual', 't2qual', 'gp', 'result', 'tempo', 'possessions', 't1pts', 
# 't2pts', 'winner', 'loser', 't1adjt', 't2adjt', 't1adjo', 't1adjd', 't2adjo', 't2adjd',
# 'gamevalue', 'mismatch', 'blowout', 't1elite', 't2elite', 'ord_date', 't1ppp', 't2ppp', 'gameppp',
# 't1rk', 't2rk', 't1gs', 't2gs', 'gamestats', 'overtimes', 't1fun', 't2fun', 'results')
# 
# #Write for Match
# write.csv(gamedataadvanced, "gamedataadvanced.csv", row.names=FALSE)
########################################################################################################
#I made some manual changes in excel to the advanced data file now I bring it back here for the rest of cleaning

gamedataadvanced <- read_csv("gamedataadvanced.csv")

#REF TABLES
team_ref <- read_csv("MTeams.csv")
city_ref <- read_csv("Cities.csv")
conf_ref <- read_csv("Conferences.csv")
region_ref <- read_csv("MSeasons.csv")
seed_ref <- read_csv("MNCAATourneySeeds.csv")
adv_id <- read_csv("AdvTeams.csv",col_names = FALSE)

#Game Data
city_results <- read_csv("MGameCities.csv")
tourney_box_results <- read_csv("MNCAATourneyDetailedResults.csv")
regular_box_results <- read_csv("MRegularSeasonDetailedResults.csv")
prob <- read_csv("markov_opp.csv")

adv_id <- rename(adv_id, winner = X1)
gamedataadvanced <- merge(gamedataadvanced, adv_id, by="winner")
gamedataadvanced <- rename(gamedataadvanced, WinId = X2)
adv_id <- rename(adv_id, loser = winner)
gamedataadvanced <- merge(gamedataadvanced, adv_id, by="loser")
gamedataadvanced <- rename(gamedataadvanced, LoseId = X2)

tourney_box_results$Type <- "Tourney"
regular_box_results$Type <- "Regular"

gamedataregular <- bind_rows(regular_box_results,tourney_box_results)
gamedataadvanced$join1 <- paste(gamedataadvanced$date,gamedataadvanced$WinId,gamedataadvanced$LoseId)
gamedataregular$join1 <- paste(gamedataregular$date,gamedataregular$WTeamID,gamedataregular$LTeamID)

full <- merge(gamedataadvanced, gamedataregular, by='join1', all.x=TRUE)

tidy <- full %>% select(winner,loser,date.x,conmatch,prediction,team1,t1adjo,t1adjd,team2,t2adjo,t2adjd,Season,conf,Type,WTeamSeed,LTeamSeed,City)
tidy$date.x <- as.POSIXct(tidy$date.x, format="%m/%d/%Y")
#Creating seeding example
i=2011
ts <- {}
ts2 <- {}
ets <- {}
ets2 <- {}
seed <- {}
seed2 <- {}
yearw <- {}
while (i <= 2019) {
  tourney <- tidy %>%
    group_by(winner,loser,WTeamSeed,LTeamSeed) %>%
    filter(Season == i & conf == 3) %>%
    ungroup()
  tourney <- tourney %>%
    arrange(date.x)
  tourney <- tourney[5:52,]
  tourney$WTeamSeed <- substr(tourney$WTeamSeed, 1, 3)
  tourney$LTeamSeed <- substr(tourney$LTeamSeed, 1, 3)
  list1 <- tourney$winner
  list2 <- tourney$loser
  appended_list <- append(list1, list2)
  appended_list <- as.data.frame(appended_list)
  appended_list <- unique(appended_list)
  
  df1 <- dplyr::select(tourney,winner,WTeamSeed)
  df1 <- rename(df1, team = winner,seed = WTeamSeed)
  df2 <- select(tourney,loser,LTeamSeed)
  df2 <- rename(df2, team = loser,seed= LTeamSeed)
  df3 <- bind_rows(df1,df2)
  
  appended_list <- rename(appended_list, team = appended_list)
  appended_list <- merge(appended_list, df3, by='team', all.x=TRUE)
  tourney_teams <- unique(appended_list)
  
  pred <- {}
  pred2 <- {}
  pred3 <- {}
  for (z in tourney_teams$team) {
    regular1 <- tidy %>%
      group_by(team1,date.x) %>%
      filter(Season == i & (conf == 1 | conf == 0) & team1 == z) %>%
      ungroup() %>%
      select(t1adjo,t1adjd,date.x)
    
    regular2 <- tidy %>%
      group_by(team2,date.x) %>%
      filter(Season == i & (conf == 1 | conf == 0) & team2 == z) %>%
      ungroup() %>%
      select(t2adjo,t2adjd,date.x)
    regular1 <- rename(regular1, adjo = t1adjo,adjd = t1adjd)
    regular2 <- rename(regular2, adjo = t2adjo,adjd = t2adjd)
    regular3 <- bind_rows(regular1,regular2)
    regular3 <- regular3 %>%
      arrange(date.x)
    
    regular3$adj <- regular3$adjo - regular3$adjd
    
    myts <- ts(regular3$adj, frequency=1)
    MA <- auto.arima(myts,allowdrift = TRUE,allowmean = TRUE, seasonal = FALSE)
    MA2 <- ets(myts,model = "ZZN", allow.multiplicative.trend = TRUE)
    
    # ts.plot(myts)
    # points(MA$fitted, type = "l", col = 2, lty = 2)
    # points(MA2$fitted, type = "l", col = 2, lty = 2)
    
    predict_MA <- forecast(MA, h = 1)
    predict_MA2 <- forecast(MA2, h = 1)
    pred <- append(pred,predict_MA$mean[1])
    pred2 <- append(pred2,predict_MA2$mean[1])
    
  }
  tourney_teams$pred1 <- parse_number(tourney_teams$seed)
  tourney_teams$pred2 <- pred
  tourney_teams$pred2 <- tourney_teams$pred2 + (1-min(tourney_teams$pred2))
  tourney_teams$pred3 <- pred2
  tourney_teams$pred3 <- tourney_teams$pred3 + (1-min(tourney_teams$pred3))
  
  tourney_teams <- tourney_teams %>% distinct()
  
  
  year <- paste("tourney",as.character(i), sep = "")
  assign(year,tourney_teams)
  
  mkv <- merge(prob, tourney_teams %>% select(seed, team, pred1), by.x = "teamseed", by.y = "seed")
  mkv <- rename(mkv, prob1 = pred1)
  mkv <- merge(mkv, tourney_teams %>% select(seed, pred1), by.x = "opp1", by.y = "seed")
  mkv <- rename(mkv, prob2 = pred1)
  mkv <- merge(mkv, tourney_teams %>% select(seed, pred1), by.x = "opp2", by.y = "seed")
  mkv <- rename(mkv, prob3 = pred1)
  mkv <- merge(mkv, tourney_teams %>% select(seed, pred1), by.x = "opp3", by.y = "seed")
  mkv <- rename(mkv, prob4 = pred1)
  
  mkv$pr1 <- mkv$prob2 / (mkv$prob1 + mkv$prob2)
  mkv$pr2 <- mkv$pr1*(((mkv$prob4/(mkv$prob3 + mkv$prob4))*(mkv$prob3/(mkv$prob1 + mkv$prob3)))+((mkv$prob3/(mkv$prob3 + mkv$prob4))*(mkv$prob4/(mkv$prob1 + mkv$prob4))))
  mkv <- mkv %>%
    select(team,teamseed,pr1,pr2)
  year <- paste("seed",as.character(i), sep = "")
  assign(year,mkv)
  
  winseed <- merge(prob, mkv %>% select(teamseed, pr1) , by.x = "teamseed", by.y = "teamseed")
  winseed <- rename(winseed, t1 = pr1)
  winseed <- merge(winseed, mkv %>% select(teamseed, pr1) , by.x = "opp1", by.y = "teamseed")
  winseed <- rename(winseed, t2 = pr1)
  winseed$winr1 <- ifelse((winseed$t1 > winseed$t2),winseed$teamseed,ifelse((winseed$t2 > winseed$t1),winseed$opp1,0))
  winseed <- select(winseed, teamseed, opp1, opp2, opp3, winr1)
  winseed <- merge(winseed, mkv %>% select(teamseed, pr2) , by.x = "teamseed", by.y = "teamseed")
  winseed <- rename(winseed, t1 = pr2)
  winseed <- merge(winseed, mkv %>% select(teamseed, pr2) , by.x = "opp1", by.y = "teamseed")
  winseed <- rename(winseed, t2 = pr2)
  winseed <- merge(winseed, mkv %>% select(teamseed, pr2) , by.x = "opp2", by.y = "teamseed")
  winseed <- rename(winseed, t3 = pr2)
  winseed <- merge(winseed, mkv %>% select(teamseed, pr2) , by.x = "opp3", by.y = "teamseed")
  winseed <- rename(winseed, t4 = pr2)
  winseed$winr2 <- ifelse((winseed$t1 > winseed$t2)&(winseed$t1 > winseed$t3)&(winseed$t1 > winseed$t4),winseed$teamseed,ifelse((winseed$t2 > winseed$t1)&(winseed$t2 > winseed$t3)&(winseed$t2 > winseed$t4),winseed$opp1,ifelse((winseed$t3 > winseed$t1)&(winseed$t3 > winseed$t2)&(winseed$t3 > winseed$t4),winseed$opp2,ifelse((winseed$t4 > winseed$t1)&(winseed$t4 > winseed$t2)&(winseed$t4 > winseed$t3),winseed$opp3,0))))
  winseed <- select(winseed, winr1, winr2)
  uniqueseedr1 <- as.data.frame(unique(winseed$winr1))
  uniqueseedr1 <- rename(uniqueseedr1, winr1 = "unique(winseed$winr1)")
  uniqueseedr1$test <- 1
  uniqueseedr2 <- as.data.frame(unique(winseed$winr2))
  uniqueseedr2 <- rename(uniqueseedr2, winr2 = "unique(winseed$winr2)")
  uniqueseedr2$test <- 1
  
  
  test <- tourney %>%
    arrange(date.x)
  test <- test[1:32,]
  test <- merge(test, uniqueseedr1, by.x = "WTeamSeed", by.y = "winr1", all.x = T)
  test$test <- ifelse(is.na(test$test),0,test$test)
  testseed <- sum(test$test)/32
  
  year <- paste("seed",as.character(i),"r1", sep = "")
  assign(year,testseed)
  year <- paste("predictseedr1",as.character(i), sep = "")
  assign(year,test)
  
  
  test <- tourney %>%
    arrange(date.x)
  test <- test[33:48,]
  test <- merge(test, uniqueseedr2, by.x = "WTeamSeed", by.y = "winr2", all.x = T)
  
  test$test <- ifelse(is.na(test$test),0,test$test)
  testseed2 <- sum(test$test)/16
  
  year <- paste("seed",as.character(i),"r2", sep = "")
  assign(year,testseed2)
  year <- paste("predictseedr2",as.character(i), sep = "")
  assign(year,test)
  
  
  mkv2 <- merge(prob, tourney_teams %>% select(seed, team, pred2), by.x = "teamseed", by.y = "seed")
  mkv2 <- rename(mkv2, prob1 = pred2)
  mkv2 <- merge(mkv2, tourney_teams %>% select(seed, pred2), by.x = "opp1", by.y = "seed")
  mkv2 <- rename(mkv2, prob2 = pred2)
  mkv2 <- merge(mkv2, tourney_teams %>% select(seed, pred2), by.x = "opp2", by.y = "seed")
  mkv2 <- rename(mkv2, prob3 = pred2)
  mkv2 <- merge(mkv2, tourney_teams %>% select(seed, pred2), by.x = "opp3", by.y = "seed")
  mkv2 <- rename(mkv2, prob4 = pred2)
  
  mkv2$pr1 <- mkv2$prob1 / (mkv2$prob1 + mkv2$prob2)
  mkv2$pr2 <- mkv2$pr1*(((mkv2$prob3/(mkv2$prob3 + mkv2$prob4))*(mkv2$prob1/(mkv2$prob1 + mkv2$prob3)))+((mkv2$prob4/(mkv2$prob3 + mkv2$prob4))*(mkv2$prob1/(mkv2$prob1 + mkv2$prob4))))
  mkv2 <- mkv2 %>%
    select(team,teamseed,pr1,pr2)
  year <- paste("ts",as.character(i), sep = "")
  assign(year,mkv2)
  
  
  wints <- merge(prob, mkv2 %>% select(teamseed, pr1) , by.x = "teamseed", by.y = "teamseed")
  wints <- rename(wints, t1 = pr1)
  wints <- merge(wints, mkv2 %>% select(teamseed, pr1) , by.x = "opp1", by.y = "teamseed")
  wints <- rename(wints, t2 = pr1)
  wints$winr1 <- ifelse((wints$t1 > wints$t2),wints$teamseed,ifelse((wints$t2 > wints$t1),wints$opp1,0))
  wints <- select(wints, teamseed, opp1, opp2, opp3, winr1)
  wints <- merge(wints, mkv2 %>% select(teamseed, pr2) , by.x = "teamseed", by.y = "teamseed")
  wints <- rename(wints, t1 = pr2)
  wints <- merge(wints, mkv2 %>% select(teamseed, pr2) , by.x = "opp1", by.y = "teamseed")
  wints <- rename(wints, t2 = pr2)
  wints <- merge(wints, mkv2 %>% select(teamseed, pr2) , by.x = "opp2", by.y = "teamseed")
  wints <- rename(wints, t3 = pr2)
  wints <- merge(wints, mkv2 %>% select(teamseed, pr2) , by.x = "opp3", by.y = "teamseed")
  wints <- rename(wints, t4 = pr2)
  wints$winr2 <- ifelse((wints$t1 > wints$t2)&(wints$t1 > wints$t3)&(wints$t1 > wints$t4),wints$teamseed,ifelse((wints$t2 > wints$t1)&(wints$t2 > wints$t3)&(wints$t2 > wints$t4),wints$opp1,ifelse((wints$t3 > wints$t1)&(wints$t3 > wints$t2)&(wints$t3 > wints$t4),wints$opp2,ifelse((wints$t4 > wints$t1)&(wints$t4 > wints$t2)&(wints$t4 > wints$t3),wints$opp3,0))))
  wints <- select(wints, winr1, winr2)
  uniquetsr1 <- as.data.frame(unique(wints$winr1))
  uniquetsr1 <- rename(uniquetsr1, winr1 = "unique(wints$winr1)")
  uniquetsr1$test <- 1
  uniquetsr2 <- as.data.frame(unique(wints$winr2))
  uniquetsr2 <- rename(uniquetsr2, winr2 = "unique(wints$winr2)")
  uniquetsr2$ind <- parse_number(uniquetsr2$winr2)
  uniquetsr2$ind <- ifelse(uniquetsr2$ind >= 5 ,1,0)
  upset <- sum(uniquetsr2$ind)
  year <- paste("upsetts",as.character(i), sep = "")
  assign(year,upset)
  uniquetsr2 <- select(uniquetsr2, winr2)
  uniquetsr2$test <- 1
  
  
  test <- tourney %>%
    arrange(date.x)
  test <- test[1:32,]
  test <- merge(test, uniquetsr1, by.x = "WTeamSeed", by.y = "winr1", all.x = T)
  test$test <- ifelse(is.na(test$test),0,test$test)
  testts <- sum(test$test)/32
  year <- paste("ts",as.character(i),"r1", sep = "")
  assign(year,testts)
  year <- paste("predicttsr1",as.character(i), sep = "")
  assign(year,test)
  
  
  
  test <- tourney %>%
    arrange(date.x)
  test <- test[33:48,]
  test <- merge(test, uniquetsr2, by.x = "WTeamSeed", by.y = "winr2", all.x = T)
  test$test <- ifelse(is.na(test$test),0,test$test)
  
  testts2 <- sum(test$test)/16
  year <- paste("ts",as.character(i),"r2", sep = "")
  assign(year,testts2)
  year <- paste("predicttsr2",as.character(i), sep = "")
  assign(year,test)
  
  
  mkv3 <- merge(prob, tourney_teams %>% select(seed, team, pred3), by.x = "teamseed", by.y = "seed")
  mkv3 <- rename(mkv3, prob1 = pred3)
  mkv3 <- merge(mkv3, tourney_teams %>% select(seed, pred3), by.x = "opp1", by.y = "seed")
  mkv3 <- rename(mkv3, prob2 = pred3)
  mkv3 <- merge(mkv3, tourney_teams %>% select(seed, pred3), by.x = "opp2", by.y = "seed")
  mkv3 <- rename(mkv3, prob3 = pred3)
  mkv3 <- merge(mkv3, tourney_teams %>% select(seed, pred3), by.x = "opp3", by.y = "seed")
  mkv3 <- rename(mkv3, prob4 = pred3)
  
  mkv3$pr1 <- mkv3$prob1 / (mkv3$prob1 + mkv3$prob2)
  mkv3$pr2 <- mkv3$pr1*(((mkv3$prob3/(mkv3$prob3 + mkv3$prob4))*(mkv3$prob1/(mkv3$prob1 + mkv3$prob3)))+((mkv3$prob4/(mkv3$prob3 + mkv3$prob4))*(mkv3$prob1/(mkv3$prob1 + mkv3$prob4))))
  mkv3 <- mkv3 %>%
    select(team,teamseed,pr1,pr2)
  year <- paste("ets",as.character(i), sep = "")
  assign(year,mkv3)
  
  winets <- merge(prob, mkv3 %>% select(teamseed, pr1) , by.x = "teamseed", by.y = "teamseed")
  winets <- rename(winets, t1 = pr1)
  winets <- merge(winets, mkv3 %>% select(teamseed, pr1) , by.x = "opp1", by.y = "teamseed")
  winets <- rename(winets, t2 = pr1)
  winets$winr1 <- ifelse((winets$t1 > winets$t2),winets$teamseed,ifelse((winets$t2 > winets$t1),winets$opp1,0))
  winets <- select(winets, teamseed, opp1, opp2, opp3, winr1)
  winets <- merge(winets, mkv3 %>% select(teamseed, pr2) , by.x = "teamseed", by.y = "teamseed")
  winets <- rename(winets, t1 = pr2)
  winets <- merge(winets, mkv3 %>% select(teamseed, pr2) , by.x = "opp1", by.y = "teamseed")
  winets <- rename(winets, t2 = pr2)
  winets <- merge(winets, mkv3 %>% select(teamseed, pr2) , by.x = "opp2", by.y = "teamseed")
  winets <- rename(winets, t3 = pr2)
  winets <- merge(winets, mkv3 %>% select(teamseed, pr2) , by.x = "opp3", by.y = "teamseed")
  winets <- rename(winets, t4 = pr2)
  winets$winr2 <- ifelse((winets$t1 > winets$t2)&(winets$t1 > winets$t3)&(winets$t1 > winets$t4),winets$teamseed,ifelse((winets$t2 > winets$t1)&(winets$t2 > winets$t3)&(winets$t2 > winets$t4),winets$opp1,ifelse((winets$t3 > winets$t1)&(winets$t3 > winets$t2)&(winets$t3 > winets$t4),winets$opp2,ifelse((winets$t4 > winets$t1)&(winets$t4 > winets$t2)&(winets$t4 > winets$t3),winets$opp3,0))))
  winets <- select(winets, winr1, winr2)
  uniqueetsr1 <- as.data.frame(unique(winets$winr1))
  uniqueetsr1 <- rename(uniqueetsr1, winr1 = "unique(winets$winr1)")
  uniqueetsr1$test <- 1
  uniqueetsr2 <- as.data.frame(unique(winets$winr2))
  uniqueetsr2 <- rename(uniqueetsr2, winr2 = "unique(winets$winr2)")
  uniqueetsr2$ind <- parse_number(uniqueetsr2$winr2)
  uniqueetsr2$ind <- ifelse(uniqueetsr2$ind >= 5 ,1,0)
  upset <- sum(uniqueetsr2$ind)
  year <- paste("upsetets",as.character(i), sep = "")
  assign(year,upset)
  uniqueetsr2 <- select(uniqueetsr2, winr2)
  uniqueetsr2$test <- 1
  
  
  test <- tourney %>%
    arrange(date.x)
  test <- test[1:32,]
  test <- merge(test, uniqueetsr1, by.x = "WTeamSeed", by.y = "winr1", all.x = T)
  test$test <- ifelse(is.na(test$test),0,test$test)
  testets <- sum(test$test)/32
  year <- paste("ets",as.character(i),"r1", sep = "")
  assign(year,testets)
  year <- paste("predictetsr1",as.character(i), sep = "")
  assign(year,test)
  
  test <- tourney %>%
    arrange(date.x)
  test <- test[33:48,]
  test <- merge(test, uniqueetsr2, by.x = "WTeamSeed", by.y = "winr2", all.x = T)
  test$test <- ifelse(is.na(test$test),0,test$test)
  
  testets2 <- sum(test$test)/16
  year <- paste("ets",as.character(i),"r2", sep = "")
  assign(year,testets2)
  year <- paste("predictetsr2",as.character(i), sep = "")
  assign(year,test)
  
  ts<-append(ts,testts)
  ts2<-append(ts2,testts2)
  ets<-append(ets,testets)
  ets2<-append(ets2,testets2)
  seed<-append(seed,testseed)
  seed2<-append(seed2,testseed2)
  yearw <- append(yearw,i) 
  
  
  i = i + 1
}

compare_results <- data.frame(yearw)
compare_results$ts <- ts
compare_results$ts2 <- ts2
compare_results$ets <- ets
compare_results$ets2 <- ets2
compare_results$seed <- seed
compare_results$seed2 <- seed2
compare_results$tsnum <- (ts*32) + (ts2*16)
compare_results$etsnum <- (ets*32) + (ets2*16)
compare_results$seednum <- (seed*32) + (seed2*16)