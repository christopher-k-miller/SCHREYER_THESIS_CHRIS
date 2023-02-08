library(dplyr)
library(tidyverse)
library(data.table)
library(forecast)

rm(list=ls())

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

tidy <- full %>% select(winner,loser,date.x,conmatch,prediction,team1,t1adjo,t1adjd,t1rk,team2,t2adjo,t2adjd,t2rk,Season,NumOT,Type,WTeamSeed,LTeamSeed,City)
tidy$t1gs <- as.numeric(tidy$t1gs)
tidy$t2gs <- as.numeric(tidy$t2gs)
#Creating seeding example
i=2011
r1w <- {}
r2w <- {}
rall <- {}
yearw <- {}
while (i <= 2019) {
tourney <- tidy %>%
  group_by(winner,loser,WTeamSeed,LTeamSeed) %>%
  filter(Season == i & Type == "Tourney") %>%
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

for (z in tourney_teams$team) {
regular1 <- tidy %>%
  group_by(team1,date.x) %>%
  filter(Season == i & Type == "Regular" & team1 == z) %>%
  ungroup() %>%
  select(t1adjo,t1adjd,date.x,t1rk)

regular2 <- tidy %>%
  group_by(team2,date.x) %>%
  filter(Season == i & Type == "Regular" & team2 == z) %>%
  ungroup() %>%
  select(t2adjo,t2adjd,date.x,t2rk)
regular1 <- rename(regular1, adjo = t1adjo,adjd = t1adjd,rk = t1rk)
regular2 <- rename(regular2, adjo = t2adjo,adjd = t2adjd,rk = t2rk)
regular3 <- bind_rows(regular1,regular2)
regular3 <- regular3 %>%
  arrange(date.x)

regular3$adj <- regular3$adjo - regular3$adjd
# regular3$rk2 <- (358-regular3$rk)/358
# regular3$adj2 <- regular3$adj*regular3$rk2
myts <- ts(regular3$adj, frequency=1)
MA <- auto.arima(myts,allowdrift = TRUE,allowmean = TRUE, seasonal = FALSE)

# ts.plot(myts)
# points(MA$fitted, type = "l", col = 2, lty = 2)

predict_MA <- forecast(MA, h = 1)

pred <- append(pred,predict_MA$mean[1])
}
tourney_teams$pred1 <- parse_number(tourney_teams$seed)
tourney_teams$pred2 <- pred
tourney_teams$pred2 <- tourney_teams$pred2
tourney_teams$pred3 <- ((17-tourney_teams$pred1)/17)*tourney_teams$pred2
tourney_teams <- left_join(tourney_teams, tourney %>% select(team1,t1rk),by = c("team"="team1"))
tourney_teams <- left_join(tourney_teams, tourney %>% select(team2,t2rk),by = c("team"="team2"))
tourney_teams$t2rk <- ifelse(is.na(tourney_teams$t2rk),tourney_teams$t1rk,tourney_teams$t2rk)
tourney_teams$t1rk <- ifelse(is.na(tourney_teams$t1rk),tourney_teams$t2rk,tourney_teams$t1rk)
tourney_teams$pred4 <- (tourney_teams$t1rk+tourney_teams$t2rk)/2
tourney_teams <- tourney_teams %>% distinct()
tourney_teams <- select(tourney_teams,subset = -c(t1rk,t2rk))

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

test <- merge(tourney, mkv %>% select(teamseed,pr1), by.x = "WTeamSeed", by.y = "teamseed")
test <- rename(test, pr1.x = pr1)
test <- merge(test, mkv %>% select(teamseed,pr1), by.x = "LTeamSeed", by.y = "teamseed")
test <- rename(test, pr1.y = pr1)
test <- test %>%
  arrange(date.x)
test <- test[1:32,]
test$predwinner <- ifelse(test$pr1.x > test$pr1.y, test$winner,test$loser)
test$test <- ifelse(test$predwinner == test$winner, 1, 0)
testseed <- sum(test$test)/32
year <- paste("seed",as.character(i),"r1", sep = "")
assign(year,testseed)
test$test2 <- ifelse(test$test == 0, test$winner, NA)
remove <- test$test2
remove <-remove[!is.na(remove)]
mkvremove <- mkv[ !mkv$team %in% remove, ]

test <- merge(tourney, mkvremove %>% select(teamseed,pr2), by.x = "WTeamSeed", by.y = "teamseed",all.x =TRUE)
test <- rename(test, pr2.x = pr2)
test <- merge(test, mkvremove %>% select(teamseed,pr2), by.x = "LTeamSeed", by.y = "teamseed",all.x =TRUE)
test <- rename(test, pr2.y = pr2)
test <- test %>%
  arrange(date.x)
test <- test[33:48,]
test$pr2.xind <- ifelse(is.na(test$pr2.x),1,0)
test$pr2.yind <- ifelse(is.na(test$pr2.y),1,0)
test <- subset(test, select=-c(pr2.x,pr2.y))
test <- merge(test, mkv %>% select(teamseed,pr2), by.x = "WTeamSeed", by.y = "teamseed")
test <- rename(test, pr2.x = pr2)
test <- merge(test, mkv %>% select(teamseed,pr2), by.x = "LTeamSeed", by.y = "teamseed")
test <- rename(test, pr2.y = pr2)
test <- test %>%
  arrange(date.x)
test$predwinner <- ifelse(test$pr2.x > test$pr2.y, test$winner,test$loser)
test$test <- ifelse((test$predwinner == test$winner & (test$pr2.x > test$pr2.y) & (test$pr2.xind != 1)) | (test$predwinner == test$winner & (test$pr2.y > test$pr2.x) & (test$pr2.yind != 1)), 1, 0)
testseed2 <- sum(test$test)/16
year <- paste("seed",as.character(i),"r2", sep = "")
assign(year,testseed2)





test <- merge(tourney, mkv2 %>% select(teamseed,pr1), by.x = "WTeamSeed", by.y = "teamseed")
test <- rename(test, pr1.x = pr1)
test <- merge(test, mkv2 %>% select(teamseed,pr1), by.x = "LTeamSeed", by.y = "teamseed")
test <- rename(test, pr1.y = pr1)
test <- test %>%
  arrange(date.x)
test <- test[1:32,]
test$predwinner <- ifelse(test$pr1.x > test$pr1.y, test$winner,test$loser)
test$test <- ifelse(test$predwinner == test$winner, 1, 0)
testts <- sum(test$test)/32
year <- paste("ts",as.character(i),"r1", sep = "")
assign(year,testts)
test$test2 <- ifelse(test$test == 0, test$winner, NA)
remove <- test$test2
remove <-remove[!is.na(remove)]
mkvremove <- mkv2[ !mkv2$team %in% remove, ]


test <- merge(tourney, mkvremove %>% select(teamseed,pr2), by.x = "WTeamSeed", by.y = "teamseed",all.x =TRUE)
test <- rename(test, pr2.x = pr2)
test <- merge(test, mkvremove %>% select(teamseed,pr2), by.x = "LTeamSeed", by.y = "teamseed",all.x =TRUE)
test <- rename(test, pr2.y = pr2)
test <- test %>%
  arrange(date.x)
test <- test[33:48,]
test$pr2.xind <- ifelse(is.na(test$pr2.x),1,0)
test$pr2.yind <- ifelse(is.na(test$pr2.y),1,0)
test <- subset(test, select=-c(pr2.x,pr2.y))
test <- merge(test, mkv2 %>% select(teamseed,pr2), by.x = "WTeamSeed", by.y = "teamseed")
test <- rename(test, pr2.x = pr2)
test <- merge(test, mkv2 %>% select(teamseed,pr2), by.x = "LTeamSeed", by.y = "teamseed")
test <- rename(test, pr2.y = pr2)
test <- test %>%
  arrange(date.x)
test$predwinner <- ifelse(test$pr2.x > test$pr2.y, test$winner,test$loser)
test$test <- ifelse((test$predwinner == test$winner & (test$pr2.x > test$pr2.y) & (test$pr2.xind != 1)) | (test$predwinner == test$winner & (test$pr2.y > test$pr2.x) & (test$pr2.yind != 1)), 1, 0)
testts2 <- sum(test$test)/16
year <- paste("ts",as.character(i),"r2", sep = "")
assign(year,testts2)

#Method 3


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
year <- paste("blend",as.character(i), sep = "")
assign(year,mkv3)

test <- merge(tourney, mkv3 %>% select(teamseed,pr1), by.x = "WTeamSeed", by.y = "teamseed")
test <- rename(test, pr1.x = pr1)
test <- merge(test, mkv3 %>% select(teamseed,pr1), by.x = "LTeamSeed", by.y = "teamseed")
test <- rename(test, pr1.y = pr1)
test <- test %>%
  arrange(date.x)
test <- test[1:32,]
test$predwinner <- ifelse(test$pr1.x > test$pr1.y, test$winner,test$loser)
test$test <- ifelse(test$predwinner == test$winner, 1, 0)
testblend <- sum(test$test)/32
year <- paste("blend",as.character(i),"r1", sep = "")
assign(year,testblend)
test$test2 <- ifelse(test$test == 0, test$winner, NA)
remove <- test$test2
remove <-remove[!is.na(remove)]
mkvremove <- mkv3[ !mkv3$team %in% remove, ]


test <- merge(tourney, mkvremove %>% select(teamseed,pr2), by.x = "WTeamSeed", by.y = "teamseed",all.x =TRUE)
test <- rename(test, pr2.x = pr2)
test <- merge(test, mkvremove %>% select(teamseed,pr2), by.x = "LTeamSeed", by.y = "teamseed",all.x =TRUE)
test <- rename(test, pr2.y = pr2)
test <- test %>%
  arrange(date.x)
test <- test[33:48,]
test$pr2.xind <- ifelse(is.na(test$pr2.x),1,0)
test$pr2.yind <- ifelse(is.na(test$pr2.y),1,0)
test <- subset(test, select=-c(pr2.x,pr2.y))
test <- merge(test, mkv3 %>% select(teamseed,pr2), by.x = "WTeamSeed", by.y = "teamseed")
test <- rename(test, pr2.x = pr2)
test <- merge(test, mkv3 %>% select(teamseed,pr2), by.x = "LTeamSeed", by.y = "teamseed")
test <- rename(test, pr2.y = pr2)
test <- test %>%
  arrange(date.x)
test$predwinner <- ifelse(test$pr2.x > test$pr2.y, test$winner,test$loser)
test$test <- ifelse((test$predwinner == test$winner & (test$pr2.x > test$pr2.y) & (test$pr2.xind != 1)) | (test$predwinner == test$winner & (test$pr2.y > test$pr2.x) & (test$pr2.yind != 1)), 1, 0)
testblend2 <- sum(test$test)/16
year <- paste("blend",as.character(i),"r2", sep = "")
assign(year,testblend2)

#TARV


mkv4 <- merge(prob, tourney_teams %>% select(seed, team, pred4), by.x = "teamseed", by.y = "seed")
mkv4 <- rename(mkv4, prob1 = pred4)
mkv4 <- merge(mkv4, tourney_teams %>% select(seed, pred4), by.x = "opp1", by.y = "seed")
mkv4 <- rename(mkv4, prob2 = pred4)
mkv4 <- merge(mkv4, tourney_teams %>% select(seed, pred4), by.x = "opp2", by.y = "seed")
mkv4 <- rename(mkv4, prob3 = pred4)
mkv4 <- merge(mkv4, tourney_teams %>% select(seed, pred4), by.x = "opp3", by.y = "seed")
mkv4 <- rename(mkv4, prob4 = pred4)

mkv4$pr1 <- mkv4$prob2 / (mkv4$prob1 + mkv4$prob2)
mkv4$pr2 <- mkv4$pr1*(((mkv4$prob4/(mkv4$prob3 + mkv4$prob4))*(mkv4$prob3/(mkv4$prob1 + mkv4$prob3)))+((mkv4$prob3/(mkv4$prob3 + mkv4$prob4))*(mkv4$prob4/(mkv4$prob1 + mkv4$prob4))))
mkv4 <- mkv4 %>%
  select(team,teamseed,pr1,pr2)
year <- paste("tarv",as.character(i), sep = "")
assign(year,mkv4)

test <- merge(tourney, mkv4 %>% select(teamseed,pr1), by.x = "WTeamSeed", by.y = "teamseed")
test <- rename(test, pr1.x = pr1)
test <- merge(test, mkv4 %>% select(teamseed,pr1), by.x = "LTeamSeed", by.y = "teamseed")
test <- rename(test, pr1.y = pr1)
test <- test %>%
  arrange(date.x)
test <- test[1:32,]
test$predwinner <- ifelse(test$pr1.x > test$pr1.y, test$winner,test$loser)
test$test <- ifelse(test$predwinner == test$winner, 1, 0)
testtarv <- sum(test$test)/32
year <- paste("tarv",as.character(i),"r1", sep = "")
assign(year,testtarv)
test$test2 <- ifelse(test$test == 0, test$winner, NA)
remove <- test$test2
remove <-remove[!is.na(remove)]
mkvremove <- mkv4[ !mkv4$team %in% remove, ]


test <- merge(tourney, mkvremove %>% select(teamseed,pr2), by.x = "WTeamSeed", by.y = "teamseed",all.x =TRUE)
test <- rename(test, pr2.x = pr2)
test <- merge(test, mkvremove %>% select(teamseed,pr2), by.x = "LTeamSeed", by.y = "teamseed",all.x =TRUE)
test <- rename(test, pr2.y = pr2)
test <- test %>%
  arrange(date.x)
test <- test[33:48,]
test$pr2.xind <- ifelse(is.na(test$pr2.x),1,0)
test$pr2.yind <- ifelse(is.na(test$pr2.y),1,0)
test <- subset(test, select=-c(pr2.x,pr2.y))
test <- merge(test, mkv4 %>% select(teamseed,pr2), by.x = "WTeamSeed", by.y = "teamseed")
test <- rename(test, pr2.x = pr2)
test <- merge(test, mkv4 %>% select(teamseed,pr2), by.x = "LTeamSeed", by.y = "teamseed")
test <- rename(test, pr2.y = pr2)
test <- test %>%
  arrange(date.x)
test$predwinner <- ifelse(test$pr2.x > test$pr2.y, test$winner,test$loser)
test$test <- ifelse((test$predwinner == test$winner & (test$pr2.x > test$pr2.y) & (test$pr2.xind != 1)) | (test$predwinner == test$winner & (test$pr2.y > test$pr2.x) & (test$pr2.yind != 1)), 1, 0)
testtarv2 <- sum(test$test)/16
year <- paste("tarv",as.character(i),"r2", sep = "")
assign(year,testtarv2)



if (testts >= testseed){
r1w<-append(r1w,1)
} else{
r1w<-append(r1w,0)  
}
if (testts2 >= testseed2){
r2w<-append(r2w,1)
}  else {
r2w<-append(r2w,0)  
} 
if (((testts*32)+(testts2*16)) >= ((testseed*32)+(testseed2*16))){
  rall<-append(rall,1)
}  else {
  rall<-append(rall,0)  
}  
yearw <- append(yearw,i)  

i = i + 1
}

compare_results <- data.frame(yearw)
compare_results$r1w <- r1w
compare_results$r2w <- r2w
compare_results$rall <- rall

