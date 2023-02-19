#Analysis

library(kableExtra)
library(reshape2)
library(ggplot2)
library(scales)


#EDA



#TS





#Analysis Tables and Figuress

probmethodtable <- as.data.frame(cbind(seed2011$team,seed2011$teamseed,round(seed2011$pr1,2),round(seed2011$pr2,2),round(ts2017$pr1,2),round(ts2017$pr2,2),
                                       round(ets2017$pr1,2),round(ets2017$pr2,2)))
probmethodtable <- probmethodtable %>% 
  arrange(V2)
probmethodtable <- probmethodtable[1:16,]
names(probmethodtable) <- c('Team','Seed','Seed Method R2','Seed Method R3','ARIMA Method R2','ARIMA Method R3','ETS Method R2','ETS Method R3')
probmethodtable$Seed <- parse_number(probmethodtable$Seed)
probmethodtable %>%
  kbl(caption = "Probabilites of Making Round 2 and 3 using Markov Chain") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(2, border_right = TRUE)%>%
  column_spec(4, border_right = TRUE)%>%
  column_spec(6, border_right = TRUE)


seed <- rbind(seed2011,seed2012,seed2013,seed2014,seed2015,seed2016,seed2017,seed2018,seed2019)
seed$teamseed <- parse_number(seed$teamseed)
seed <- seed %>%
  group_by(teamseed)%>%
  summarise('Seed Method R2' = mean(pr1))
ts <- rbind(ts2011,ts2012,ts2013,ts2014,ts2015,ts2016,ts2017,ts2018,ts2019)
ts$teamseed <- parse_number(ts$teamseed)
ts <- ts %>%
  group_by(teamseed)%>%
  summarise('ARIMA Method R2' = mean(pr1))
ets <- rbind(ets2011,ets2012,ets2013,ets2014,ets2015,ets2016,ets2017,ets2018,ets2019)
ets$teamseed <- parse_number(ets$teamseed)
ets <- ets %>%
  group_by(teamseed)%>%
  summarise('ETS Method R2' = mean(pr1))
seed <- left_join(seed,ts)
seed <- left_join(seed,ets)
seed$teamseed <- as.character(seed$teamseed)
seed <- melt(seed,id = "teamseed")
seed$teamseed <- as.numeric(seed$teamseed)
ggplot(seed, aes(x=teamseed, y=value, group=variable)) +
  geom_line(aes(linetype=variable))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position="top")+
  scale_x_continuous(breaks = round(seq(min(seed$teamseed), max(seed$teamseed), by = 1),1)) +
  scale_y_continuous(breaks = round(seq(min(seed$value), max(seed$value), by = 0.1),1)) + 
  labs(x = "Seed",y="Proabability of Making R2",title="Comparing Methods Seed Dynamic")

pctresults <- compare_results %>%
  select(-c(tsnum,seednum,etsnum))
pctresults <- pctresults[, c(1, 6, 2, 4, 7, 3, 5)]
names(pctresults) <- c('Year','Seed Method R2 % Correct','ARIMA Method R2 % Correct','ETS Method R2 % Correct','Seed Method R3 % Correct','ARIMA Method R3 % Correct','ETS Method R3 % Correct')
pctresults[2:7] <- sapply(pctresults[2:7], function(x) percent(x, accuracy=.01))

pctresults %>%
  kbl(caption = "Percent Correct in R2 and R3") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(1, border_right = TRUE)%>%
  column_spec(4, border_right = TRUE)

numresults <- compare_results %>%
  select(yearw,tsnum,seednum,etsnum)
numresults <- numresults[, c(1, 3, 2, 4)]
names(numresults) <- c('Year','Seed Method Games Correct','ARIMA Method Games Correct','ETS Method Games Correct')

numresults %>%
  kbl(caption = "Number of Games Correctly Predicted") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(1, border_right = TRUE)

k <- 2011
y <- list()
x <- list()
n <- list()
o <- list()
while (k <= 2019){
  sname <- paste("predicttsr1",as.character(k), sep = "")
  dname <- get(paste("predicttsr1",as.character(k), sep = ""))
  x[[sname]] <- dname
  sname <- paste("predictetsr1",as.character(k), sep = "")
  dname <- get(paste("predictetsr1",as.character(k), sep = ""))
  y[[sname]] <- dname
  k <- k+1
}
k <- 2011
while (k <= 2019){
  sname <- paste("predicttsr2",as.character(k), sep = "")
  dname <- get(paste("predicttsr2",as.character(k), sep = ""))
  n[[sname]] <- dname
  sname <- paste("predictetsr2",as.character(k), sep = "")
  dname <- get(paste("predictetsr2",as.character(k), sep = ""))
  o[[sname]] <- dname
  k <- k+1
}


tsupsetcorrectr1 <- {}
tsupsetincorrectr1 <- {}
etsupsetcorrectr1 <- {}
etsupsetincorrectr1 <- {}
tsupsetcorrectr2 <- {}
tsupsetincorrectr2 <- {}
etsupsetcorrectr2 <- {}
etsupsetincorrectr2 <- {}
for (i in x) {
i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
i$predupset <- ifelse(((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr1.x > i$pr1.y)),1,0)
i$upsetcorrect <- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)


sumupsetcorrect <- sum(i$upsetcorrect)
sumupsetincorrect <- sum(i$upsetincorrect)

tsupsetcorrectr1 <-append(tsupsetcorrectr1,sumupsetcorrect)
tsupsetincorrectr1 <-append(tsupsetincorrectr1,sumupsetincorrect)
}
for (i in y) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr1.x > i$pr1.y) ,1 , 0)
  i$upsetcorrect <- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  sumupsetcorrect <- sum(i$upsetcorrect)
  sumupsetincorrect <- sum(i$upsetincorrect)  
  
  etsupsetcorrectr1 <-append(etsupsetcorrectr1,sumupsetcorrect)
  etsupsetincorrectr1 <-append(etsupsetincorrectr1,sumupsetincorrect)
}
for (i in n) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr2.x > i$pr2.y)& (test$pr2.xind != 1) ,1 , 0)
  i$upsetcorrect <- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  sumupsetcorrect <- sum(i$upsetcorrect)
  sumupsetincorrect <- sum(i$upsetincorrect)  
  
  tsupsetcorrectr2 <-append(tsupsetcorrectr2,sumupsetcorrect)
  tsupsetincorrectr2 <-append(tsupsetincorrectr2,sumupsetincorrect)
}
for (i in o) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr2.x > i$pr2.y)& (test$pr2.xind != 1) ,1 , 0)
  i$upsetcorrect <- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  sumupsetcorrect <- sum(i$upsetcorrect)
  sumupsetincorrect <- sum(i$upsetincorrect)  
  
  etsupsetcorrectr2 <-append(etsupsetcorrectr2,sumupsetcorrect)
  etsupsetincorrectr2 <-append(etsupsetincorrectr2,sumupsetincorrect)
}

