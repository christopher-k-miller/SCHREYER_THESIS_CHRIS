#Analysis

library(kableExtra)
library(reshape2)
library(ggplot2)
library(scales)


#EDA

regular1 <- tidy %>%
  group_by(team1,date.x) %>%
  filter(Season == 2011 & (conf == 1 | conf == 0) & team1 == "Penn St.") %>%
  ungroup() %>%
  select(t1adjo,t1adjd,date.x)

regular2 <- tidy %>%
  group_by(team2,date.x) %>%
  filter(Season == 2011 & (conf == 1 | conf == 0) & team2 == "Penn St.") %>%
  ungroup() %>%
  select(t2adjo,t2adjd,date.x)
regular1 <- rename(regular1, adjo = t1adjo,adjd = t1adjd)
regular2 <- rename(regular2, adjo = t2adjo,adjd = t2adjd)
regular3 <- bind_rows(regular1,regular2)
regular3 <- regular3 %>%
  arrange(date.x)

regular3$adj <- regular3$adjo - regular3$adjd

myts <- ts(regular3$adj, frequency=1)
MA <- auto.arima(myts,allowdrift = TRUE,allowmean = TRUE, seasonal = FALSE, trace = T)
MA2 <- ets(myts, model = "ZZN", allow.multiplicative.trend = TRUE)

ts.plot(myts, ylab = "Penn State Efficiency")
myts %>% diff() %>% ggtsdisplay(main="")
ts.plot(myts, ylab = "Penn State Efficiency")
points(MA$fitted, type = "l", col = 2, lty = 2)
ts.plot(myts, ylab = "Penn State Efficiency")
points(MA2$fitted, type = "l", col = 2, lty = 2)
summary(MA2)




#Analysis Tables and Figuress

probmethodtable <- as.data.frame(cbind(seed2011$team,seed2011$teamseed,round(seed2011$pr1,2),round(seed2011$pr2,2),round(ts2017$pr1,2),round(ts2017$pr2,2),
                                       round(ets2017$pr1,2),round(ets2017$pr2,2)))

probmethodtable <- probmethodtable %>% 
  arrange(V2)
probmethodtable <- probmethodtable[1:16,]
names(probmethodtable) <- c('Team','Seed','Seed Method R1','Seed Method R2','ARIMA Method R1','ARIMA Method R2','ETS Method R1','ETS Method R2')
probmethodtable$Seed <- parse_number(probmethodtable$Seed)
probmethodtable %>%
  kbl(caption = "Probabilites of Making Round 1 and 2 using Markov Chain") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(2, border_right = TRUE)%>%
  column_spec(4, border_right = TRUE)%>%
  column_spec(6, border_right = TRUE)


seed <- rbind(seed2011,seed2012,seed2013,seed2014,seed2015,seed2016,seed2017,seed2018,seed2019)
seed$teamseed <- parse_number(seed$teamseed)
seed <- seed %>%
  group_by(teamseed)%>%
  summarise('Seed Method R1' = mean(pr1))
ts <- rbind(ts2011,ts2012,ts2013,ts2014,ts2015,ts2016,ts2017,ts2018,ts2019)
ts$teamseed <- parse_number(ts$teamseed)
ts <- ts %>%
  group_by(teamseed)%>%
  summarise('ARIMA Method R1' = mean(pr1))
ets <- rbind(ets2011,ets2012,ets2013,ets2014,ets2015,ets2016,ets2017,ets2018,ets2019)
ets$teamseed <- parse_number(ets$teamseed)
ets <- ets %>%
  group_by(teamseed)%>%
  summarise('ETS Method R1' = mean(pr1))
seed <- left_join(seed,ts)
seed <- left_join(seed,ets)
seed$teamseed <- as.character(seed$teamseed)
historic <- c(0.99,	0.94,	0.85,	0.78,	0.65,	0.63,	0.60,	0.49,	0.51,	0.40,	0.38,	0.35,	0.22,	0.15,	0.06,	0.01)
seed$'Historic Prob.' <- historic
seed <- melt(seed,id = "teamseed")
seed$teamseed <- as.numeric(seed$teamseed)
ggplot(seed, aes(x=teamseed, y=value, color=variable)) +
  geom_line(aes(linetype=variable))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position="top")+
  scale_x_continuous(breaks = round(seq(min(seed$teamseed), max(seed$teamseed), by = 1),1)) +
  scale_y_continuous(breaks = round(seq(min(seed$value), max(seed$value), by = 0.1),1)) + 
  labs(x = "Seed",y="Proabability of Winning R1",title="Comparing Methods Seed Dynamic")

pctresults <- compare_results %>%
  select(-c(tsnum,seednum,etsnum))
pctresults <- pctresults[, c(1, 6, 2, 4, 7, 3, 5)]
names(pctresults) <- c('Year','Seed Method R1 % Correct','ARIMA Method R1 % Correct','ETS Method R1 % Correct','Seed Method R2 % Correct','ARIMA Method R2 % Correct','ETS Method R2 % Correct')
pctresults[2:7] <- sapply(pctresults[2:7], function(x) percent(x, accuracy=.01))

pctresults %>%
  kbl(caption = "Percent Correct in R1 and R2") %>%
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

mean(numresults$`Seed Method Games Correct`)
mean(numresults$`ARIMA Method Games Correct`)
mean(numresults$`ETS Method Games Correct`)
sd(numresults$`Seed Method Games Correct`)
sd(numresults$`ARIMA Method Games Correct`)
sd(numresults$`ETS Method Games Correct`)

mean((parse_number(pctresults$`Seed Method R1 % Correct`)/100)*32)
mean((parse_number(pctresults$`ARIMA Method R1 % Correct`)/100)*32)
mean((parse_number(pctresults$`ETS Method R1 % Correct`)/100)*32)

mean((parse_number(pctresults$`Seed Method R2 % Correct`)/100)*16)
mean((parse_number(pctresults$`ARIMA Method R2 % Correct`)/100)*16)
mean((parse_number(pctresults$`ETS Method R2 % Correct`)/100)*16)


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
etsupsetcorrectr1 <- {}
tsupsetcorrectr2 <- {}
etsupsetcorrectr2 <- {}
for (i in x) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr1.x > i$pr1.y)|(parse_number(i$LTeamSeed)>parse_number(i$WTeamSeed))&(i$pr1.y > i$pr1.x) ,1 , 0)
  i$upsetcorrect<- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  
  sumupsetcorrect <- sum(i$upsetincorrect)+sum(i$upsetcorrect)
  
  
  tsupsetcorrectr1 <-append(tsupsetcorrectr1,sumupsetcorrect)

}
for (i in y) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr1.x > i$pr1.y)|(parse_number(i$LTeamSeed)>parse_number(i$WTeamSeed))&(i$pr1.y > i$pr1.x) ,1 , 0)
  i$upsetcorrect<- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  
  
  sumupsetcorrect <- sum(i$upsetincorrect)+sum(i$upsetcorrect)
  
  etsupsetcorrectr1 <-append(etsupsetcorrectr1,sumupsetcorrect)
}
for (i in n) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr2.x > i$pr2.y)& (test$pr2.xind != 1)|(parse_number(i$LTeamSeed)>parse_number(i$WTeamSeed))&(i$pr2.y > i$pr2.x)& (test$pr2.yind != 1) ,1 , 0)
  i$upsetcorrect<- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  
  sumupsetcorrect <- sum(i$upsetincorrect)+sum(i$upsetcorrect)
  
  tsupsetcorrectr2 <-append(tsupsetcorrectr2,sumupsetcorrect)
}
for (i in o) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr2.x > i$pr2.y)& (test$pr2.xind != 1)|(parse_number(i$LTeamSeed)>parse_number(i$WTeamSeed))&(i$pr2.y > i$pr2.x)& (test$pr2.yind != 1) ,1 , 0)
  i$upsetcorrect<- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  
  sumupsetcorrect <- sum(i$upsetincorrect)+sum(i$upsetcorrect)
  
  etsupsetcorrectr2 <-append(etsupsetcorrectr2,sumupsetcorrect)
}

tsupset <- tsupsetcorrectr1 + tsupsetcorrectr2
etsupset <- etsupsetcorrectr1 + etsupsetcorrectr2

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
etsupsetcorrectr1 <- {}
tsupsetcorrectr2 <- {}
etsupsetcorrectr2 <- {}
for (i in x) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr1.x > i$pr1.y)|(parse_number(i$LTeamSeed)>parse_number(i$WTeamSeed))&(i$pr1.y > i$pr1.x) ,1 , 0)
  i$upsetcorrect<- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  
  sumupsetcorrect <- sum(i$upsetcorrect)
  
  
  tsupsetcorrectr1 <-append(tsupsetcorrectr1,sumupsetcorrect)
  
}
for (i in y) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr1.x > i$pr1.y)|(parse_number(i$LTeamSeed)>parse_number(i$WTeamSeed))&(i$pr1.y > i$pr1.x) ,1 , 0)
  i$upsetcorrect<- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  
  
  sumupsetcorrect <- sum(i$upsetcorrect)
  
  etsupsetcorrectr1 <-append(etsupsetcorrectr1,sumupsetcorrect)
}
for (i in n) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr2.x > i$pr2.y)& (test$pr2.xind != 1)|(parse_number(i$LTeamSeed)>parse_number(i$WTeamSeed))&(i$pr2.y > i$pr2.x)& (test$pr2.yind != 1) ,1 , 0)
  i$upsetcorrect<- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  
  sumupsetcorrect <- sum(i$upsetcorrect)
  
  tsupsetcorrectr2 <-append(tsupsetcorrectr2,sumupsetcorrect)
}
for (i in o) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr2.x > i$pr2.y)& (test$pr2.xind != 1)|(parse_number(i$LTeamSeed)>parse_number(i$WTeamSeed))&(i$pr2.y > i$pr2.x)& (test$pr2.yind != 1) ,1 , 0)
  i$upsetcorrect<- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  
  sumupsetcorrect <- sum(i$upsetcorrect)
  
  etsupsetcorrectr2 <-append(etsupsetcorrectr2,sumupsetcorrect)
  

}

tsupsetcorrect <- tsupsetcorrectr1 + tsupsetcorrectr2
etsupsetcorrect <- etsupsetcorrectr1 + etsupsetcorrectr2





upsetpct <- as.data.frame(yearw)
upsetpct <- rename(upsetpct,Year = yearw)
upsetpct$'ARIMA Method % Correct' <- percent(tsupsetcorrect/tsupset,.01)
upsetpct$'ETS Method % Correct' <- percent(etsupsetcorrect/etsupset,.01)

upsetpct %>%
  kbl(caption = "Percent of Upset Games Correctly Predicted") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(1, border_right = TRUE)

mean(tsupsetcorrect/tsupset)
mean(etsupsetcorrect/etsupset)



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
etsupsetcorrectr1 <- {}
tsupsetcorrectr2 <- {}
etsupsetcorrectr2 <- {}
for (i in x) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr1.x > i$pr1.y)|(parse_number(i$LTeamSeed)>parse_number(i$WTeamSeed))&(i$pr1.y > i$pr1.x) ,1 , 0)
  i$upsetcorrect<- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  
  sumupsetcorrect <- sum(i$predupset)
  
  
  tsupsetcorrectr1 <-append(tsupsetcorrectr1,sumupsetcorrect)
  
}
for (i in y) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr1.x > i$pr1.y)|(parse_number(i$LTeamSeed)>parse_number(i$WTeamSeed))&(i$pr1.y > i$pr1.x) ,1 , 0)
  i$upsetcorrect<- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  
  
  sumupsetcorrect <- sum(i$predupset)
  
  etsupsetcorrectr1 <-append(etsupsetcorrectr1,sumupsetcorrect)
}
for (i in n) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr2.x > i$pr2.y)& (test$pr2.xind != 1)|(parse_number(i$LTeamSeed)>parse_number(i$WTeamSeed))&(i$pr2.y > i$pr2.x)& (test$pr2.yind != 1) ,1 , 0)
  i$upsetcorrect<- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  
  sumupsetcorrect <- sum(i$predupset)
  
  tsupsetcorrectr2 <-append(tsupsetcorrectr2,sumupsetcorrect)
}
for (i in o) {
  i$upset <- ifelse(parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed),1,0)
  i$predupset <- ifelse((parse_number(i$WTeamSeed)>parse_number(i$LTeamSeed))&(i$pr2.x > i$pr2.y)& (test$pr2.xind != 1)|(parse_number(i$LTeamSeed)>parse_number(i$WTeamSeed))&(i$pr2.y > i$pr2.x)& (test$pr2.yind != 1) ,1 , 0)
  i$upsetcorrect<- ifelse( i$upset == 1 & i$predupset == 1 ,1,0)
  i$upsetincorrect <- ifelse( i$upset == 1 & i$predupset == 0,1,0)
  
  
  sumupsetcorrect <- sum(i$predupset)
  
  etsupsetcorrectr2 <-append(etsupsetcorrectr2,sumupsetcorrect)
  

}

tspredupset <- tsupsetcorrectr1 + tsupsetcorrectr2
etspredupset <- etsupsetcorrectr1 + etsupsetcorrectr2


upsetpct2 <- as.data.frame(yearw)
upsetpct2 <- rename(upsetpct2,Year = yearw)
upsetpct2$'ARIMA Method % Correct' <- percent(tsupsetcorrect/tspredupset,.01)
upsetpct2$'ETS Method % Correct' <- percent(etsupsetcorrect/etspredupset,.01)

upsetpct2 %>%
  kbl(caption = "Percent of Predicted Upsets Correct") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(1, border_right = TRUE)

mean(tsupsetcorrect/tspredupset)
mean(etsupsetcorrect/etspredupset)
sd(tsupsetcorrect/tspredupset)
sd(etsupsetcorrect/etspredupset)
mean(tspredupset)
mean(etspredupset)


k <- 2011
y <- list()
x <- list()
z <- list()
n <- list()
o <- list()
p <- list()
while (k <= 2019){
  sname <- paste("predicttsr1",as.character(k), sep = "")
  dname <- get(paste("predicttsr1",as.character(k), sep = ""))
  x[[sname]] <- dname
  sname <- paste("predictetsr1",as.character(k), sep = "")
  dname <- get(paste("predictetsr1",as.character(k), sep = ""))
  y[[sname]] <- dname
  sname <- paste("predictseedr1",as.character(k), sep = "")
  dname <- get(paste("predictseedr1",as.character(k), sep = ""))
  z[[sname]] <- dname
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
  sname <- paste("predictseedr2",as.character(k), sep = "")
  dname <- get(paste("predictseedr2",as.character(k), sep = ""))
  p[[sname]] <- dname
  k <- k+1
}



tsupsetcorrectr1 <- {}
etsupsetcorrectr1 <- {}
seedupsetcorrectr1 <- {}
tsupsetcorrectr2 <- {}
etsupsetcorrectr2 <- {}
seedupsetcorrectr2 <- {}
for (i in x) {
  i$diff <- abs(parse_number(i$WTeamSeed)-parse_number(i$LTeamSeed))
  i$correct<- ifelse( (i$test == 1)&(i$diff <= 7) ,1,0)
  
  
  sumupsetcorrect <- sum(i$correct)
  
  
  tsupsetcorrectr1 <-append(tsupsetcorrectr1,sumupsetcorrect)
  
}
for (i in y) {
  i$diff <- abs(parse_number(i$WTeamSeed)-parse_number(i$LTeamSeed))
  i$correct<- ifelse( (i$test == 1)&(i$diff <= 7) ,1,0)
  
  
  
  sumupsetcorrect <- sum(i$correct)
  
  etsupsetcorrectr1 <-append(etsupsetcorrectr1,sumupsetcorrect)
}
for (i in z) {
  i$diff <- abs(parse_number(i$WTeamSeed)-parse_number(i$LTeamSeed))
  i$correct<- ifelse( (i$test == 1)&(i$diff <= 7) ,1,0)
  
  
  
  sumupsetcorrect <- sum(i$correct)
  
  seedupsetcorrectr1 <-append(seedupsetcorrectr1,sumupsetcorrect)
}
for (i in n) {
  i$diff <- abs(parse_number(i$WTeamSeed)-parse_number(i$LTeamSeed))
  i$correct<- ifelse( (i$test == 1)&(i$diff <= 7) ,1,0)
  
  
  sumupsetcorrect <- sum(i$correct)
  
  tsupsetcorrectr2 <-append(tsupsetcorrectr2,sumupsetcorrect)
}
for (i in o) {
  i$diff <- abs(parse_number(i$WTeamSeed)-parse_number(i$LTeamSeed))
  i$correct<- ifelse( (i$test == 1)&(i$diff <= 7) ,1,0)
  
  
  sumupsetcorrect <- sum(i$correct)
  
  etsupsetcorrectr2 <-append(etsupsetcorrectr2,sumupsetcorrect)
  
  
}
for (i in p) {
  i$diff <- abs(parse_number(i$WTeamSeed)-parse_number(i$LTeamSeed))
  i$correct<- ifelse( (i$test == 1)&(i$diff <= 7) ,1,0)
  
  
  sumupsetcorrect <- sum(i$correct)
  
  seedupsetcorrectr2 <-append(seedupsetcorrectr2,sumupsetcorrect)
  
  
}

tsnewwintotal <- tsupsetcorrectr1 + tsupsetcorrectr2 
etsnewwintotal <- etsupsetcorrectr1 + etsupsetcorrectr2 
seednewwintotal <- seedupsetcorrectr1 + seedupsetcorrectr2 

newwintotal <- as.data.frame(yearw)
newwintotal <- rename(newwintotal,Year = yearw)
newwintotal$'Seed Close Matchup Games Correct' <- seednewwintotal
newwintotal$'ARIMA Close Matchup Games Correct' <- tsnewwintotal
newwintotal$'ETS Close Matchup Games Correct' <- etsnewwintotal

newwintotal %>%
  kbl(caption = "Close Matchup Games Correct") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(1, border_right = TRUE)

mean(newwintotal$'Seed Close Matchup Games Correct')
mean(newwintotal$'ARIMA Close Matchup Games Correct')
mean(newwintotal$'ETS Close Matchup Games Correct')

