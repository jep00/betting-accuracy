#### (2) ANALYSING THE ACCURACY OF BETTING ON ENG/SCO FOOTBALL LEAGUES
#----
#N.B.:
#assume the working directory and libraries are set as in eliteleagues.R
#else run:
#setwd("~/Desktop/University/University Year 3/331MP/Data");rm(list=ls())
library(car); library(MASS); library(ggplot2); library(gridExtra)

### READING DATA ----
#We downloading data straight from football-data.co.uk

divisions <- c("E0", "E1", "E2", "E3", "EC", "SC0", "SC1", "SC2", "SC3")
levels <- c("1", "2", "3")
seasons <- c("0506", "0607", "0708", "0809", "0910", "1011", "1112", "1213", "1314", "1415", "1516", "1617", "1718", "1819", "1920")

enscoTemp <- NULL; ensco <- NULL
for (i in seasons){
  for (j in divisions){
    enscoTemp <- read.csv(paste0("https://www.football-data.co.uk/mmz4281/", i, "/", j, ".csv"), fileEncoding="latin1")
    enscoTemp$Season <- with(enscoTemp, i)
    enscoTemp$Div <- with(enscoTemp, j)
    if (i=="1920"){
      enscoTemp$BbAvH <- enscoTemp$AvgH
      enscoTemp$BbAvA <- enscoTemp$AvgA
      enscoTemp$BbAvD <- enscoTemp$AvgD
      enscoTemp$BbAv.2.5 <- enscoTemp$Avg.2.5 
      enscoTemp$BbAv.2.5.1 <- enscoTemp$Avg.2.5.1
      enscoTemp$BbAvAHH <- enscoTemp$AvgAHH
      enscoTemp$BbAvAHA <- enscoTemp$AvgAHA
      enscoTemp$BbAHh <- enscoTemp$AHh}
    else{}
    enscoTemp$Over2.5Odds <- enscoTemp$BbAv.2.5
    enscoTemp$Under2.5Odds <- enscoTemp$BbAv.2.5.1
    #Greater Than or Less Than don't copy through: 
    #A manual check confirms this is the right way round
    enscoTemp$HomeHandicap <- enscoTemp$BbAHh
    enscoTemp <- enscoTemp[,c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "BbAvH", "BbAvD", "BbAvA", "Over2.5Odds", "Under2.5Odds", "HomeHandicap", "BbAvAHH", "BbAvAHA", "Season")]
    ensco <- rbind(ensco, enscoTemp)
  }
}
ensco<-na.omit(ensco)

### NORMALISING PROBS, FINDING WINNING BETS ----

#Defining the league 'level':   1 - Elite (EPL, SPL, Championship);
#                               2 - Fully Professional Lower Leagues;
#                               3 - Semi-Professional Lower Leagues.
ensco$Level<-with(ensco, rep(0, nrow(ensco)))
for (k in 1:(nrow(ensco))){
  if (ensco$Div[k]=="E0"){ensco$Level[k]<-1}
  else if (ensco$Div[k]=="E1"){ensco$Level[k]<-1}
  else if (ensco$Div[k]=="E2"){ensco$Level[k]<-2}
  else if (ensco$Div[k]=="E3"){ensco$Level[k]<-2}
  else if (ensco$Div[k]=="EC"){ensco$Level[k]<-3}
  else if (ensco$Div[k]=="SC0"){ensco$Level[k]<-1}
  else if (ensco$Div[k]=="SC1"){ensco$Level[k]<-2}
  else if (ensco$Div[k]=="SC2"){ensco$Level[k]<-3}
  else if (ensco$Div[k]=="SC3"){ensco$Level[k]<-3}
  else{}
}

#Adding and normalising Probability columns
#Pre Normalised:-
  #1X2 Market
    ensco$AvgHProbPN <- with(ensco, round(1/BbAvH, 4))
    ensco$AvgDProbPN <- with(ensco, round(1/BbAvD, 4))
    ensco$AvgAProbPN <- with(ensco, round(1/BbAvA, 4))
  #Under/Over 2.5 goals market
    ensco$Over2.5ProbPN <-  with(ensco, round(1/Over2.5Odds, 4))
    ensco$Under2.5ProbPN <- with(ensco, round(1/Under2.5Odds, 4))
  #Asian Handicap Markets
    ensco$AH.HProbPN <- with(ensco, round(1/BbAvAHH, 4))
    ensco$AH.AProbPN <- with(ensco, round(1/BbAvAHA, 4))
#Finding Overrounds:-
    ensco$OneXTwoOverround <- with(ensco, (AvgHProbPN + AvgDProbPN + AvgAProbPN))
    ensco$UnderOverOverround <- with(ensco, (Over2.5ProbPN + Under2.5ProbPN))
    ensco$AHOverround <- with(ensco, (AH.HProbPN + AH.AProbPN))
#Normalising
    ensco$AvgHProb <- with(ensco, round(AvgHProbPN/OneXTwoOverround, 4))
    ensco$AvgDProb <- with(ensco, round(AvgDProbPN/OneXTwoOverround, 4))
    ensco$AvgAProb <- with(ensco, round(AvgAProbPN/OneXTwoOverround, 4))
    ensco$Over2.5Prob <- with(ensco, round(Over2.5ProbPN/UnderOverOverround, 4))
    ensco$Under2.5Prob <- with(ensco, round(Under2.5ProbPN/UnderOverOverround, 4))
    ensco$AH.HProb <- with(ensco, round(AH.HProbPN/AHOverround, 4))
    ensco$AH.AProb <- with(ensco, round(AH.AProbPN/AHOverround, 4))
    
#A few important notes:
#Rangers had a -275 goal handicap vs. East Fife; assume this meant -2.75:
ensco$HomeHandicap[ensco$HomeTeam=="Rangers" & ensco$Date=="11/01/14"] <- -2.75
#Hamilton had a 12.5 goal handicap vs. Rangers; assume this meant 1.25:
ensco$HomeHandicap[ensco$HomeTeam=="Hamilton" & ensco$Date=="25/10/08"] <- 1.25
    
#"Correct" Results -- Straight forward for the 1X2 and U/O Markets:
N = nrow(ensco)
ensco$Correct1X2 <- with(ensco, rep(0,N))
ensco$IncorrectA1X2 <- with(ensco, rep(0,N))
ensco$IncorrectB1X2 <- with(ensco, rep(0,N))
ensco$TotGoals <- with(ensco, FTHG + FTAG)
ensco$CorrectUO <- with(ensco, rep(0,N))
ensco$IncorrectUO <- with(ensco, rep(0,N))

for (l in 1:N){
  if (ensco$FTR[l] == "H"){
    ensco$Correct1X2[l] <- ensco$Correct1X2[l] + ensco$AvgHProb[l]
    ensco$IncorrectA1X2[l] <- ensco$IncorrectA1X2[l] + ensco$AvgAProb[l]
    ensco$IncorrectB1X2[l] <- ensco$IncorrectB1X2[l] + ensco$AvgDProb[l]}
  else if (ensco$FTR[l] == "D"){
    ensco$Correct1X2[l] <- ensco$Correct1X2[l] + ensco$AvgDProb[l]
    ensco$IncorrectA1X2[l] <- ensco$IncorrectA1X2[l] + ensco$AvgAProb[l]
    ensco$IncorrectB1X2[l] <- ensco$IncorrectB1X2[l] + ensco$AvgHProb[l]}
  else if (ensco$FTR[l] == "A"){
    ensco$Correct1X2[l] <- ensco$Correct1X2[l] + ensco$AvgAProb[l]
    ensco$IncorrectA1X2[l] <- ensco$IncorrectA1X2[l] + ensco$AvgHProb[l]
    ensco$IncorrectB1X2[l] <- ensco$IncorrectB1X2[l] + ensco$AvgDProb[l]}
  else{}
  if (ensco$TotGoals[l] > 2.5){
    ensco$CorrectUO[l] <- ensco$CorrectUO[l] + ensco$Over2.5Prob[l]
    ensco$IncorrectUO[l] <- ensco$IncorrectUO[l] + ensco$Under2.5Prob[l]}
  else if (ensco$TotGoals[l] < 2.5){
    ensco$CorrectUO[l] <- ensco$CorrectUO[l] + ensco$Under2.5Prob[l]
    ensco$IncorrectUO[l] <- ensco$IncorrectUO[l] + ensco$Over2.5Prob[l]}
  else{}
}

ensco$uo.res <- NULL
for (b in 1:N){
  if (ensco$TotGoals[b] > 2.5){ensco$uo.res[b] <- "over"}
  else {ensco$uo.res[b] <- "under"}
}

#For Asian Handicaps, we need to work out the winner(s)
ensco$FTHG.ah <- with(ensco, rep(0,N))
for (m in 1:N){ensco$FTHG.ah[m] <- ensco$FTHG[m] + ensco$HomeHandicap[m]}

#There are 3 types of AH bets: Assume the bookie bets on the HOME team
#   Integer: e.g. +1 Handicap for home team
          ##  EVENT                                   #Winner
          # - If there is a draw, or home team wins   Home
          # - If away teams by 1 goal                 Void (stake refund)
          # - If away team wins by more than 1 goal   Away
#   Half: e.g. +0.5 Handicap for the home team
          # - If the home team wins, or it is a draw  Home wins
          # - If the away team wins                   Away
#   Quarter: e.g. +0.75 Handicap for the home team
#         Half the stake goes to +1, Half goes to +0.5
          # - If the home team wins                   Home
          # - If the game is a draw                   Home wins
          # - If away team wins by 1 goal             HalfAway wins
          # - If away wins by more than 1 goal        Away wins

ensco$ah.gap <- with(ensco, FTHG.ah - FTAG); ensco$ah.res <- NULL
for (n in 1:N){
  if (ensco$ah.gap[n]<(-0.25)){ensco$ah.res[n]<-"aw"}
  else if (ensco$ah.gap[n]==(-0.25)){ensco$ah.res[n]<-"hfaw"}
  else if (ensco$ah.gap[n]==0){ensco$ah.res[n]<-"vo"}
  else if (ensco$ah.gap[n]==0.25){ensco$ah.res[n]<-"hfhm"}
  else if (ensco$ah.gap[n]>0.25){ensco$ah.res[n]<-"hm"}
  else{}
}

ensco$CorrectAH <- with(ensco, rep(0,N))
ensco$IncorrectAH <- with(ensco, rep(0,N))
#Only considering the FULL wins, rather than half.
for (l in 1:N){
  if (ensco$ah.res[l] == "hm"){
    ensco$CorrectAH[l] <- ensco$CorrectAH[l] + ensco$AH.HProb[l]
    ensco$IncorrectAH[l] <- ensco$IncorrectAH[l] + ensco$AH.AProb[l]}
  else if (ensco$ah.res[l] == "aw"){
    ensco$CorrectAH[l] <- ensco$CorrectAH[l] + ensco$AH.AProb[l]
    ensco$IncorrectAH[l] <- ensco$IncorrectAH[l] + ensco$AH.HProb[l]}
  else{}
}


### BASIC CALCULATIONS ----
#1X2:
basic.1x2 <- NULL
for (a in 1:3){basic.1x2 <- c(basic.1x2, mean(ensco$AvgHProb[ensco$Level == a]))}
for (a in 1:3){basic.1x2 <- c(basic.1x2, mean(ensco$AvgDProb[ensco$Level == a]))}
for (a in 1:3){basic.1x2 <- c(basic.1x2, mean(ensco$AvgAProb[ensco$Level == a]))}
for (a in 1:3){basic.1x2 <- c(basic.1x2, sd(ensco$AvgHProb[ensco$Level == a]))}
for (a in 1:3){basic.1x2 <- c(basic.1x2, sd(ensco$AvgDProb[ensco$Level == a]))}
for (a in 1:3){basic.1x2 <- c(basic.1x2, sd(ensco$AvgAProb[ensco$Level == a]))}

basic.1x2 <- matrix(c(basic.1x2), ncol=3, byrow=T)
colnames(basic.1x2) <- 1:3
rownames(basic.1x2) <- c("1x2 Home Mean", "1x2 Draw Mean", "1x2 Away Mean", "1x2 Home SD", "1x2 Draw SD", "1x2 Away SD")

#Under/Over:
basic.uo <- NULL
for (a in 1:3){basic.uo <- c(basic.uo, mean(ensco$Under2.5Prob[ensco$Level == a]))}
for (a in 1:3){basic.uo <- c(basic.uo, mean(ensco$Over2.5Prob[ensco$Level == a]))}
for (a in 1:3){basic.uo <- c(basic.uo, sd(ensco$Under2.5Prob[ensco$Level == a]))}
for (a in 1:3){basic.uo <- c(basic.uo, sd(ensco$Over2.5Prob[ensco$Level == a]))}

basic.uo <- matrix(c(basic.uo), ncol=3, byrow=T)
colnames(basic.uo) <- 1:3
rownames(basic.uo) <- c("Under 2.5 Mean", "Over 2.5 Mean", "Under 2.5 SD", "Over 2.5 SD")

#Asian Handicaps: 
basic.ah <- NULL
for (a in 1:3){basic.ah <- c(basic.ah, mean(ensco$AH.HProb[ensco$Level == a]))}
for (a in 1:3){basic.ah <- c(basic.ah, mean(ensco$AH.AProb[ensco$Level == a]))}
for (a in 1:3){basic.ah <- c(basic.ah, sd(ensco$AH.HProb[ensco$Level == a]))}
for (a in 1:3){basic.ah <- c(basic.ah, sd(ensco$AH.AProb[ensco$Level == a]))}

basic.ah <- matrix(c(basic.ah), ncol=3, byrow=T)
colnames(basic.ah) <- 1:3
rownames(basic.ah) <- c("AH Home Mean", "AH Away Mean", "AH Home SD", "AH Away SD")

basic.calcs <- round(rbind(basic.1x2, basic.uo, basic.ah),4)

#Observed Probabilities
obsprob.1x2tab <- round(prop.table(table(ensco$FTR, ensco$Level),2), 4)[c(1,2,3), c(1,2,3)]
obsprob.uotab <- round(prop.table(table(ensco$uo.res, ensco$Level),2),4)[c(1,2), c(1,2,3)]
obsprob.ahtab <- round(prop.table(table(ensco$ah.res, ensco$Level),2),4)[c("hm", "hfhm", "vo", "hfaw", "aw"),]

#To find the % of AH full wins that are home wins
print(nrow(ensco[ensco$ah.res == "hm",]) / (nrow(ensco[ensco$ah.res == "hm",]) + nrow(ensco[ensco$ah.res == "aw",])))

#Finding this by Level
AH.Basic.Proportions <- NULL
for (i in levels){
  tempH <- nrow(ensco[ensco$ah.res == "hm" & ensco$Level == i,])
  tempA <- nrow(ensco[ensco$ah.res == "aw" & ensco$Level == i,])
  tempProp <- tempH / (tempH + tempA)
  AH.Basic.Proportions <- c(AH.Basic.Proportions, tempProp)
}
AH.Basic.Proportions <- as.matrix(AH.Basic.Proportions, nrow = 1, ncol = 3)
rownames(AH.Basic.Proportions) <- paste("Level",levels)
AH.Basic.Proportions

basic.calcs; obsprob.1x2tab; obsprob.uotab; obsprob.ahtab

### PLOTS AND VISUAL ANALYSIS ----
#Density Plots
dens1x2ha <- ggplot(ensco, aes(x=AvgHProb, color="HW")) + geom_density() + geom_density(data=ensco, mapping=aes(x=AvgAProb, color="AW")) + scale_color_manual(name="Bet Type", values=c("HW" = "blue", "AW" = "coral")) + labs(x="Consensus Probabilitiy", y="Density", caption="English and Scottish Leagues, 2005-2020", title="Home and Away Wins in\nthe 1X2 Market") + theme_light() + coord_cartesian(xlim=c(0,1))

dens1x2d <- ggplot(ensco, aes(x=AvgDProb, color="D")) + geom_density() + labs(x="Consensus Probability", y="Density", caption="English and Scottish Leagues, 2005-2020", title="\nDraws in \nthe 1X2 Market") + scale_color_manual(name="Bet Type", values=c("D" = "green4")) + theme_light() + coord_cartesian(xlim=c(0,1))

densuo <- ggplot(ensco, aes(x=Under2.5Prob, color="U")) + geom_density() + geom_density(data=ensco, aes(x=Over2.5Prob, color="O")) + labs(x="Consensus Probability", y="Density", caption="English and Scottish Leagues, 2005-2020", title="Under/Over 2.5\nGoals Market") + scale_color_manual(name="Bet Type", values=c("U" = "red", "O" = "green")) + theme_light() + coord_cartesian(xlim=c(0,1))

densah <- ggplot(ensco, aes(x=AH.HProb, color="AH HW")) + geom_density() + geom_density(data=ensco, aes(x=AH.AProb, color="AH AW")) + labs(x="Consensus Probability", y="Density", caption="English and Scottish Leagues, 2005-2020", title="Home and Away Wins in the\nAsian Handicap Market") + scale_color_manual(name="Bet Type", values=c("AH HW" = "blue", "AH AW" = "coral")) + theme_light() + coord_cartesian(xlim=c(0,1))

# Saving the plots:
ggsave(path = "./writeup/images", filename = "ensco_01_dens1x2ha.png", plot=dens1x2ha, unit="cm", width=15, height=10)
ggsave(path = "./writeup/images", filename = "ensco_02_dens1x2d.png", plot=dens1x2d, unit="cm", width=15, height=10)
ggsave(path = "./writeup/images", filename = "ensco_03_densuo.png", plot=densuo, unit="cm", width=15, height=10)
ggsave(path = "./writeup/images", filename = "ensco_04_densah.png", plot=densah, unit="cm", width=15, height=10)

dens <- grid.arrange(dens1x2ha, dens1x2d, densuo, densah, ncol=2, nrow=2)
ggsave(path = "./writeup/images", filename = "ensco_04A_densities.png", plot=dens, unit="cm", width=20, height=15)

#Home Team Handicap v. Mean Consensus P(Home Win)
#We'd expect a higher handicap => lower P_cons(Home Win)
ensco$AvgHProb.2dp <- with(ensco, round(AvgHProb, 2))
ensco$AvgAProb.2dp <- with(ensco, round(AvgAProb, 2))

ensco$Level <- as.factor(ensco$Level)
handicap.v.hprob <- ggplot(ensco, aes(x=AvgHProb.2dp, y=HomeHandicap, color=Level)) + geom_count(show.legend = T) + scale_size_area() + theme_light() + labs(x="Consensus Probability of a Home Win (1X2)", y="Home Handicap", title="Consensus P(Home Win)\nvs. Home Handicap", caption="English and Scottish Leagues, 2005-2020")

handicap.v.1x2 <- ggplot(ensco, aes(x=AvgAProb.2dp, y=HomeHandicap, color="Away Win")) + geom_count(alpha=.5) + scale_size_area() + theme_light() + geom_count(mapping=aes(x=AvgHProb.2dp, y=HomeHandicap, color="Home Win"), alpha=.5) + labs(x="Consensus Probability", y="Home Handicap", title="Consensus P(Win, 1X2)\nvs. Home Handicap", caption="English and Scottish Leagues, 2005-2020") + scale_color_manual(name = "Bet", values = c("Home Win" = "blue", "Away Win" = "coral"))

ggsave(path = "./writeup/images", filename = "ensco_05_hprob_v_hcap.png", plot=handicap.v.hprob, unit="cm", width=15, height=10)
ggsave(path = "./writeup/images", filename = "ensco_05a_1x2_v_hcap.png", plot=handicap.v.1x2, unit="cm", width=15, height=10)


#Tile Plots for 1X2 and UO Markets
#Our highest-scoring draw was 6-6: As before, we bin 6+ (not 5) goals together:
N = nrow(ensco); ensco$FTHG.Tile<-with(ensco,rep(0,N))
ensco$FTAG.Tile<-with(ensco,rep(0,N))

for (k in 1:N){
  if ((ensco$FTHG[k])>=6){ensco$FTHG.Tile[k]<-6}
  else{ensco$FTHG.Tile[k]<-ensco$FTHG[k]}}
for (k in 1:N){
  if ((ensco$FTAG[k])>=6){ensco$FTAG.Tile[k]<-6}
  else{ensco$FTAG.Tile[k]<-ensco$FTAG[k]}}

tile.1x2 <- ggplot(ensco, aes(y=FTAG.Tile, x=FTHG.Tile)) + geom_tile(aes(fill = Correct1X2)) + scale_fill_distiller(palette = "Greens", direction = 1, name="Correct 1X2\nProbability") + theme_light() + labs(title="Result vs. Correct Consensus\nProbability in the 1X2 Market", x="Home Goals", y="Away Goals", caption="English and Scottish Leagues, 2005-2020") + scale_y_discrete(limits=factor(c(1:5, "6+"))) + scale_x_discrete(limits=factor(c(1:5, "6+"))) + geom_abline(intercept=0, slope=1) + coord_cartesian(xlim=c(0,6), ylim=c(0,6))

tile.uo <- ggplot(ensco, aes(y=FTAG.Tile, x=FTHG.Tile)) + geom_tile(aes(fill = Over2.5Prob)) + scale_fill_distiller(palette = "Paired", direction = 1, name="P(Over 2.5 Goals)") + theme_light() + labs(title="Result vs. Consensus\nProbability of Over 2.5 Goals", x="Home Goals", y="Away Goals", caption="English and Scottish Leagues, 2005-2020") + scale_y_discrete(limits=factor(c(1:5, "6+"))) + scale_x_discrete(limits=factor(c(1:5, "6+"))) + coord_cartesian(xlim=c(0,6), ylim=c(0,6)) + geom_segment(aes(x=2.5,xend=2.5,y=-5,yend=2.5),color="black") + geom_segment(aes(x=-5,xend=2.5,y=2.5,yend=2.5),color="black")

ggsave(path = "./writeup/images", filename = "ensco_06_tile_1x2.png", plot=tile.1x2, unit="cm", width=15, height=10)
ggsave(path = "./writeup/images", filename = "ensco_07_tile_uo.png", plot=tile.uo, unit="cm", width=15, height=10)
tpbinsizes.ensco <- table(ensco$FTAG.Tile, ensco$FTHG.Tile) #Bin sizes

##Under/Over v. Handicap
#Are higher handicap games (more 'obvious') likely to have higher goals?
ensco$Over2.5Prob.2dp <- with(ensco, round(Over2.5Prob, 2))
handicap.v.over2.5 <- ggplot(ensco, aes(x=Over2.5Prob.2dp, y=HomeHandicap)) + geom_count() + scale_size_area() + theme_light() + labs(x="Consensus Probability of a Over 2.5 Goals", y="Home Handicap", title="Consensus P(Over 2.5 Goals)\nvs. Handicap", caption="English and Scottish Leagues, 2005-2020")

ensco$GoalDiffTile <- with(ensco, FTAG.Tile-FTHG.Tile)
expected.v.act.difference <- ggplot(ensco, aes(y=GoalDiffTile, x=HomeHandicap)) + geom_count() + scale_size_area() + theme_light() + labs(x="Home Handicap (Expected GD)", y="Away Goals minus Home Goals (Actual GD)", title="Expected vs. Actual Goal Difference", caption="English and Scottish Leagues, 2005-2020")

ggsave(path = "./writeup/images", filename = "ensco_08_handicap_v_over.png", plot=handicap.v.over2.5, unit="cm", width=15, height=10)
ggsave(path = "./writeup/images", filename = "ensco_09_exp_v_act_goaldiff.png", plot=expected.v.act.difference, unit="cm", width=15, height=10)

### CORRELATION TESTS (Kendall's Tau, Spearman) ----
cor.test(ensco$FTHG.Tile, ensco$FTAG.Tile, method = "kendall")
cor.test(ensco$FTHG.Tile, ensco$FTAG.Tile, method = 'spearman')
#p << 0.005: Strong evidence of an association
library(DescTools)
GoodmanKruskalGamma(ensco$FTHG.Tile, ensco$FTAG.Tile, conf.level = 0.95)

### CORRELATION ANALYSIS: MODEL CREATION (Overall) ----

#Cutting and defining the levels:
ensco$AvgHProb.cut <- cut(ensco$AvgHProb, 50, include.lowest=T)
levels(ensco$AvgHProb.cut) <- tapply(ensco$AvgHProb, ensco$AvgHProb.cut, mean)
ensco$AvgDProb.cut <- cut(ensco$AvgDProb, 50, include.lowest=T)
levels(ensco$AvgDProb.cut) <- tapply(ensco$AvgDProb, ensco$AvgDProb.cut, mean)
ensco$AvgAProb.cut <- cut(ensco$AvgAProb, 50, include.lowest=T)
levels(ensco$AvgAProb.cut) <- tapply(ensco$AvgAProb, ensco$AvgAProb.cut, mean)

ensco$Over2.5Prob.cut <- cut(ensco$Over2.5Prob, 50, include.lowest=T) 
levels(ensco$Over2.5Prob.cut) <- tapply(ensco$Over2.5Prob, ensco$Over2.5Prob.cut, mean)

ensco$AH.HProb.cut <- cut(ensco$AH.HProb, 50, include.lowest=T)
levels(ensco$AH.HProb.cut) <- tapply(ensco$AH.HProb, ensco$AH.HProb.cut, mean)
ensco$AH.AProb.cut <- cut(ensco$AH.AProb, 50, include.lowest=T)
levels(ensco$AH.AProb.cut) <- tapply(ensco$AH.AProb, ensco$AH.AProb.cut, mean)

#Observed Probability for each cut:
obsprob.1x2.H <- prop.table(table(ensco$FTR, ensco$AvgHProb.cut), 2)[3,]
booprob.1x2.H <- as.numeric(names(obsprob.1x2.H))
obsprob.1x2.D <- prop.table(table(ensco$FTR, ensco$AvgDProb.cut), 2)[2,]
booprob.1x2.D <- as.numeric(names(obsprob.1x2.D))
obsprob.1x2.A <- prop.table(table(ensco$FTR, ensco$AvgAProb.cut), 2)[1,]
booprob.1x2.A <- as.numeric(names(obsprob.1x2.A))

booprob.1x2 <- c(booprob.1x2.H,booprob.1x2.D,booprob.1x2.A)
obsprob.1x2 <- c(obsprob.1x2.H,obsprob.1x2.D,obsprob.1x2.A)

obsprob.uo <- prop.table(table(ensco$uo.res, ensco$Over2.5Prob.cut), 2)[1,]
booprob.uo <- as.numeric(names(obsprob.uo))

#For AH bets, we will only take full wins:
obsprob.ah.H <- prop.table(table(ensco$ah.res, ensco$AH.HProb.cut), 2)[4,] #4 = Home
booprob.ah.H <- as.numeric(names(obsprob.ah.H))
obsprob.ah.A <- prop.table(table(ensco$ah.res, ensco$AH.AProb.cut), 2)[1,] #1 = Away
booprob.ah.A <- as.numeric(names(obsprob.ah.A))

booprob.ah <- c(booprob.ah.H, booprob.ah.A)
obsprob.ah <- c(obsprob.ah.H, obsprob.ah.A)

#Final models
model.1x2.h <- lm(obsprob.1x2.H~booprob.1x2.H)
model.1x2.d <- lm(obsprob.1x2.D~booprob.1x2.D)
model.1x2.a <- lm(obsprob.1x2.A~booprob.1x2.A)
model.1x2.o <- lm(obsprob.1x2~booprob.1x2)
model.uo <- lm(obsprob.uo~booprob.uo)
model.ah <- lm(obsprob.ah~booprob.ah)

#R Squared and RMSE values:-
rsrm.val.1x2 <- matrix(c(summary(model.1x2.h)$r.squared, summary(model.1x2.d)$r.squared, summary(model.1x2.a)$r.squared, sqrt(mean(model.1x2.h$residuals^2)), sqrt(mean(model.1x2.d$residuals^2)), sqrt(mean(model.1x2.a$residuals^2)) ), ncol = 3, nrow = 2, byrow=T, dimnames = list(c("RSq", "RMSE"), c("1X2: H", "D", "A")))
rsrm.val.uoah <- matrix(c(summary(model.uo)$r.squared, summary(model.ah)$r.squared, sqrt(mean(model.uo$residuals^2)), sqrt(mean(model.ah$residuals^2)) ), ncol = 2, nrow = 2, byrow=T, dimnames = list(c("RSq", "RMSE"), c("Under/Over", "AH")))

#These show that the odds for the U/O market, 1x2 Draws are NOT accurate

#     Model Plots ----
convobs.1x2 <- ggplot(data=NULL, aes()) + geom_smooth() +
  geom_jitter(aes(x=booprob.1x2.H, y=obsprob.1x2.H, color="1X2 Home"), size=0.75) +
  geom_smooth(aes(x=booprob.1x2.H, y=obsprob.1x2.H, color="1X2 Home"), method=lm) +
  geom_jitter(aes(x=booprob.1x2.D, y=obsprob.1x2.D, color="1X2 Draw"), size=0.75) +
  geom_smooth(aes(x=booprob.1x2.D, y=obsprob.1x2.D, color="1X2 Draw"), method=lm) +
  geom_jitter(aes(x=booprob.1x2.A, y=obsprob.1x2.A, color="1X2 Away"), size=0.75) +
  geom_smooth(aes(x=booprob.1x2.A, y=obsprob.1x2.A, color="1X2 Away"), method=lm) +
  geom_abline(intercept=0, slope=1, linetype="dashed") + theme_light() + 
  labs(x = "Bookmaker Consensus Probability", y = "Observed Probability", caption="Eng/Sco 05-20") + scale_color_manual(name="Bet Type", values=c("1X2 Home" = "blue", "1X2 Draw" = "green4", "1X2 Away" = "coral")) + coord_cartesian(xlim=c(0,1), ylim=c(0,1))
  
convobs.uo <- ggplot(data=NULL, aes()) + geom_smooth() +
  geom_jitter(aes(x=booprob.uo, y=obsprob.uo, color="Under/Over"), size=0.75) +
  geom_smooth(aes(x=booprob.uo, y=obsprob.uo, color="Under/Over"), method=lm) +
  geom_abline(intercept=0, slope=1, linetype="dashed") + theme_light() +
  labs(x = "Bookmaker Consensus Probability", y = "Observed Probability",  caption="Eng/Sco 05-20") + scale_color_manual(name="Bet Type", values=c("Under/Over" = "red")) + coord_cartesian(xlim=c(0,1), ylim=c(0,1))

convobs.ah <- ggplot(data=NULL, aes()) + geom_smooth() +
  geom_jitter(aes(x=booprob.ah.H, y=obsprob.ah.H, color="AH Home"), size=0.75) +
  geom_smooth(aes(x=booprob.ah.H, y=obsprob.ah.H, color="AH Home"), method=lm) +
  geom_jitter(aes(x=booprob.ah.A, y=obsprob.ah.A, color="AH Away"), size=0.75) +
  geom_smooth(aes(x=booprob.ah.A, y=obsprob.ah.A, color="AH Away"), method=lm) +
  geom_abline(intercept=0, slope=1, linetype="dashed") + theme_light() +
  labs(x = "Bookmaker Consensus Probability", y = "Observed Probability",  caption="Eng/Sco 05-20") + scale_color_manual(name="Bet Type", values=c("AH Home" = "blue", "AH Away" = "coral")) + coord_cartesian(xlim=c(0,1), ylim=c(0,1))

ggsave(path = "./writeup/images", filename = "ensco_10_convobs_1x2.png", plot=convobs.1x2, unit="cm", width=15, height=6)  
ggsave(path = "./writeup/images", filename = "ensco_11_convobs_uo.png", plot=convobs.uo, unit="cm", width=15, height=6)
ggsave(path = "./writeup/images", filename = "ensco_12_convobs_ah.png", plot=convobs.ah, unit="cm", width=15, height=6)

### CORRELATION ANALYSIS: PER LEVEL ----
#To view sample size:- 
for (j in levels){
  DataTemp <- ensco[ensco$Level==j,]
  print(paste0("For level ",j,", n = ",nrow(DataTemp)))
}
rsqu.level <- NULL; rmse.level <- NULL; rsqu.level.1x2 <- NULL; rmse.level.1x2 <- NULL; 
rsqu.level.uo <- NULL; rmse.level.uo <- NULL; rsqu.level.ah <- NULL; rmse.level.ah <- NULL; 
p1.level <- NULL; p2.level <- NULL; p1.uo.level <- NULL; p2.uo.level <- NULL; p1.ah.level <- NULL; p2.ah.level <- NULL
slope.level.1x2 <- NULL; slope.level.uo <- NULL; slope.level.ah <- NULL

ensco$LogCorrect1x2 <- with(ensco, log(ensco$Correct1X2))
ensco$LogCorrectUO <- with(ensco, log(ensco$CorrectUO))

#As we're taking the log and results with AH voids/half-wins, we create a subset:
ensco.ah.results <- ensco[ensco$CorrectAH > 0,]
ensco.ah.results$LogCorrectAH <- with(ensco.ah.results, log(ensco.ah.results$CorrectAH))

#n.b. We don't do a model for each -- just an overall 1x2, AH and U/O. 
#     P1 and P2 is based purely on the 1X2 market.

for (j in levels){
  dataTemp <- ensco[ensco$Level==j,]
  dataTempAH <- ensco.ah.results[ensco.ah.results$Level==j,] #For AH P Values
  dataTemp$AvgHProb.cut <- cut(dataTemp$AvgHProb, 35, include.lowest = T)
  levels(dataTemp$AvgHProb.cut) <- tapply(dataTemp$AvgHProb, dataTemp$AvgHProb.cut, mean)
  dataTemp$AvgDProb.cut <- cut(dataTemp$AvgDProb, 15, include.lowest = T)
  levels(dataTemp$AvgDProb.cut) <- tapply(dataTemp$AvgDProb, dataTemp$AvgDProb.cut, mean)
  dataTemp$AvgAProb.cut <- cut(dataTemp$AvgAProb, 35, include.lowest = T)
  levels(dataTemp$AvgAProb.cut) <- tapply(dataTemp$AvgAProb, dataTemp$AvgAProb.cut, mean)
  
  dataTemp$Over2.5Prob.cut <- cut(dataTemp$Over2.5Prob, 35, include.lowest = T)
  levels(dataTemp$Over2.5Prob.cut) <- tapply(dataTemp$Over2.5Prob, dataTemp$Over2.5Prob.cut, mean)

  dataTemp$AH.HProb.cut <- cut(dataTemp$AH.HProb, 35, include.lowest = T)
  levels(dataTemp$AH.HProb.cut) <- tapply(dataTemp$AH.HProb, dataTemp$AH.HProb.cut, mean)
  dataTemp$AH.AProb.cut <- cut(dataTemp$AH.AProb, 35, include.lowest = T)
  levels(dataTemp$AH.AProb.cut) <- tapply(dataTemp$AH.AProb, dataTemp$AH.AProb.cut, mean)
  
  obs.1x2.h <- prop.table(table(dataTemp$FTR, dataTemp$AvgHProb.cut), 2)[3,]
  obs.1x2.d <- prop.table(table(dataTemp$FTR, dataTemp$AvgDProb.cut), 2)[2,]
  obs.1x2.a <- prop.table(table(dataTemp$FTR, dataTemp$AvgAProb.cut), 2)[1,]
  obs.1x2 <- c(obs.1x2.h, obs.1x2.d, obs.1x2.a)
  
  boo.1x2.h <- as.numeric(names(obs.1x2.h))
  boo.1x2.d <- as.numeric(names(obs.1x2.d))
  boo.1x2.a <- as.numeric(names(obs.1x2.a))
  boo.1x2 <- c(boo.1x2.h, boo.1x2.d, boo.1x2.a)
  
  obs.uo <- prop.table(table(dataTemp$uo.res, dataTemp$Over2.5Prob.cut), 2)[1,]
  boo.uo <- as.numeric(names(obs.uo))
  
  obs.ah.h <- prop.table(table(dataTemp$ah.res, dataTemp$AH.HProb.cut), 2)[4,]
  obs.ah.a <- prop.table(table(dataTemp$ah.res, dataTemp$AH.AProb.cut), 2)[1,]
  obs.ah <- c(obs.ah.h, obs.ah.a)
  
  boo.ah.h <- as.numeric(names(obs.ah.h))
  boo.ah.a <- as.numeric(names(obs.ah.a))
  boo.ah <- c(boo.ah.h, boo.ah.a)
  
  modelTemp.1x2 <- lm(obs.1x2 ~ boo.1x2)
  modelTemp.uo <- lm(obs.uo ~ boo.uo)
  modelTemp.ah <- lm(obs.ah ~ boo.ah)
  par(mfrow=c(3,1))
  plot(modelTemp.1x2, 5, main = paste0("1X2 Market; Level ",j))
  plot(modelTemp.uo, 5, main = paste0("UO Market; Level ",j))
  plot(modelTemp.ah, 5, main = paste0("AH Market; Level ",j))
  par(mfrow=c(1,1))
  
  plot.1x2 <- ggplot(NULL, aes(x=boo.1x2.h, y=obs.1x2.h, color="Home")) + 
    geom_smooth(method="lm", alpha=0.3) + 
    geom_smooth(aes(x=boo.1x2.a, y=obs.1x2.a, color="Away"), method="lm", alpha=0.3) + 
    geom_smooth(aes(x=boo.1x2.d, y=obs.1x2.d, color="Draw"), method="lm", alpha=0.3) +
    geom_jitter(aes(color="Home"), shape=1) + 
    geom_jitter(aes(x=boo.1x2.a, y=obs.1x2.a, color="Away"), shape=1) + 
    geom_jitter(aes(x=boo.1x2.d, y=obs.1x2.d, color="Draw"), shape=1) + 
    geom_abline(slope=1, intercept=0, color="black", linetype="dashed") + 
    coord_cartesian(xlim=c(0,1), ylim=c(0,1)) + labs(x = paste0("1X2 Bookmaker Consensus Probabilities: Level ",j), y = NULL) + scale_color_manual(name="Bet Type", values = c("Home" = "blue", "Away" = "coral", "Draw" = "green4")) + theme_light()
  
  plot.uo <- ggplot(NULL, aes(x=boo.uo, y=obs.uo, color="Over 2.5 Goals")) + 
    geom_smooth(method="lm", alpha=0.3) + 
    geom_jitter(shape=2) + 
    geom_abline(slope=1, intercept=0, color="black", linetype="dashed") + 
    coord_cartesian(xlim=c(0,1), ylim=c(0,1)) + labs(x=paste0("UO Bookmaker Consensus Probabilities: Level ",j), y=NULL) + scale_color_manual(name="Bet Type", values=c("Over 2.5 Goals" = "red")) + theme_light()
  
  plot.ah <- ggplot(NULL, aes(x=boo.ah.h, y=obs.ah.h, color="AH Home")) +
    geom_smooth(method = "lm", alpha=0.3) + 
    geom_smooth(aes(x=boo.ah.a, y=obs.ah.a, color="AH Away"), method="lm", alpha=0.3) + geom_jitter(shape=5) + 
    geom_jitter(aes(x=boo.ah.a, y=obs.ah.a, color="AH Away"), method="lm", alpha=0.3) +
    geom_abline(slope=1, intercept=0, color="black", linetype="dashed") + 
    coord_cartesian(xlim=c(0,1), ylim=c(0,1)) + labs(x= paste0("AH Bookmaker Consensus Probabilities: Level ",j), y=NULL) + scale_color_manual(name="Bet Type", values=c("AH Home"="blue", "AH Away"="coral")) + theme_light()
  
  
  plotTemp <- grid.arrange(plot.1x2, plot.uo, plot.ah, nrow=3, ncol=1, left="Observed Probability")
  ggsave(path = "./writeup/images", filename = paste0("ensco_13_level",j,".png"), plot=plotTemp, unit="cm", width=15, height=10)
  
  
  rsqu.level.1x2 <- c(rsqu.level.1x2, round(summary(modelTemp.1x2)$r.squared, 5))
  rmse.level.1x2 <- c(rmse.level.1x2, round(sqrt(mean(modelTemp.1x2$residuals^2)), 5))
  rsqu.level.uo <- c(rsqu.level.uo, round(summary(modelTemp.uo)$r.squared, 5))
  rmse.level.uo <- c(rmse.level.uo, round(sqrt(mean(modelTemp.uo$residuals^2)), 5))
  rsqu.level.ah <- c(rsqu.level.ah, round(summary(modelTemp.ah)$r.squared, 5))
  rmse.level.ah <- c(rmse.level.ah, round(sqrt(mean(modelTemp.ah$residuals^2)), 5))
  
  p1.temp <- exp(1/(nrow(dataTemp)) * sum(dataTemp$LogCorrect1x2))
  p2.temp <- 1/(nrow(dataTemp))*sum((1 - dataTemp$Correct1X2)**2 + (dataTemp$IncorrectA1X2)**2 + (dataTemp$IncorrectB1X2)**2 )
  p1.level <- c(p1.level, round(p1.temp, 5))
  p2.level <- c(p2.level, round(p2.temp ,5))
  
  p1.uo.temp <- exp(1/(nrow(dataTemp)) * sum(dataTemp$LogCorrectUO))
  p2.uo.temp <- 1/(nrow(dataTemp))*sum((1 - dataTemp$CorrectUO)**2 + (dataTemp$IncorrectUO)**2)
  p1.uo.level <- c(p1.uo.level, round(p1.uo.temp, 5))
  p2.uo.level <- c(p2.uo.level, round(p2.uo.temp ,5))
  
  p1.ah.temp <- exp(1/(nrow(dataTempAH)) * sum(dataTempAH$LogCorrectAH))
  p2.ah.temp <- 1/(nrow(dataTempAH)) * sum((1 - dataTempAH$CorrectAH)**2 + (dataTempAH$IncorrectAH)**2)
  p1.ah.level <- c(p1.ah.level, round(p1.ah.temp, 5))
  p2.ah.level <- c(p2.ah.level, round(p2.ah.temp, 5))
  
  slope.level.1x2 <- c(slope.level.1x2, modelTemp.1x2$coefficients[2]) #Coefficients[2] is the gradient
  slope.level.uo <- c(slope.level.uo, modelTemp.uo$coefficients[2])
  slope.level.ah <- c(slope.level.ah, modelTemp.ah$coefficients[2])  
  
}
rmse.level <- matrix( c(rmse.level.1x2, rmse.level.uo, rmse.level.ah), ncol=3, byrow=T, dimnames = list(c("1x2", "UO", "AH"), levels))
rsqu.level <- matrix( c(rsqu.level.1x2, rsqu.level.uo, rsqu.level.ah), ncol=3, byrow=T, dimnames = list(c("1x2", "UO", "AH"), levels))
p.values.level <- matrix(c(p1.level, p2.level, p1.uo.level, p2.uo.level, p1.ah.level, p2.ah.level), ncol=3, byrow=T, dimnames = list(c("P1 1X2", "P2 1X2", "P1 UO", "P2 UO", "P1 AH", "P2 AH"), levels))
slope.level <- matrix(c(slope.level.1x2, slope.level.uo, slope.level.ah), ncol = 3, byrow = F, dimnames = list(levels, c("1X2", "UO", "AH")))

# ---- Comparing Seasons

rsqu.season.1x2 <- NULL; rmse.season.1x2 <- NULL; p1.season.1x2 <- NULL; p2.season.1x2 <- NULL
rsqu.season.uo <- NULL; rmse.season.uo <- NULL; p1.season.uo <- NULL; p2.season.uo <- NULL
rsqu.season.ah <- NULL; rmse.season.ah <- NULL; p1.season.ah <- NULL; p2.season.ah <- NULL
slope.season.1x2 <- NULL; slope.season.uo <- NULL; slope.season.ah <- NULL

for(i in seasons){
  dataTemp <- ensco[ensco$Season == i, ]

  dataTemp$AvgHProb.cut <- cut(dataTemp$AvgHProb, 10, include.lowest = T)
  levels(dataTemp$AvgHProb.cut) <- tapply(dataTemp$AvgHProb, dataTemp$AvgHProb.cut, mean)
  dataTemp$AvgDProb.cut <- cut(dataTemp$AvgDProb, 5, include.lowest = T)
  levels(dataTemp$AvgDProb.cut) <- tapply(dataTemp$AvgDProb, dataTemp$AvgDProb.cut, mean)
  dataTemp$AvgAProb.cut <- cut(dataTemp$AvgAProb, 10, include.lowest = T)
  levels(dataTemp$AvgAProb.cut) <- tapply(dataTemp$AvgAProb, dataTemp$AvgAProb.cut, mean)
  
  dataTemp$Over2.5Prob.cut <- cut(dataTemp$Over2.5Prob, 10, include.lowest = T)
  levels(dataTemp$Over2.5Prob.cut) <- tapply(dataTemp$Over2.5Prob, dataTemp$Over2.5Prob.cut, mean)
  
  dataTemp$AH.HProb.cut <- cut(dataTemp$AH.HProb, 10, include.lowest = T)
  levels(dataTemp$AH.HProb.cut) <- tapply(dataTemp$AH.HProb, dataTemp$AH.HProb.cut, mean)
  dataTemp$AH.AProb.cut <- cut(dataTemp$AH.AProb, 10, include.lowest = T)
  levels(dataTemp$AH.AProb.cut) <- tapply(dataTemp$AH.AProb, dataTemp$AH.AProb.cut, mean)
  
  obs.1x2.h <- prop.table(table(dataTemp$FTR, dataTemp$AvgHProb.cut), 2)[3,]
  obs.1x2.d <- prop.table(table(dataTemp$FTR, dataTemp$AvgDProb.cut), 2)[2,]
  obs.1x2.a <- prop.table(table(dataTemp$FTR, dataTemp$AvgAProb.cut), 2)[1,]
  obs.1x2 <- c(obs.1x2.h, obs.1x2.d, obs.1x2.a)
  
  boo.1x2.h <- as.numeric(names(obs.1x2.h))
  boo.1x2.d <- as.numeric(names(obs.1x2.d))
  boo.1x2.a <- as.numeric(names(obs.1x2.a))
  boo.1x2 <- c(boo.1x2.h, boo.1x2.d, boo.1x2.a)
  
  obs.uo <- prop.table(table(dataTemp$uo.res, dataTemp$Over2.5Prob.cut), 2)[1,]
  boo.uo <- as.numeric(names(obs.uo))
  
  obs.ah.h <- prop.table(table(dataTemp$ah.res, dataTemp$AH.HProb.cut), 2)[4,]
  obs.ah.a <- prop.table(table(dataTemp$ah.res, dataTemp$AH.AProb.cut), 2)[1,]
  obs.ah <- c(obs.ah.h, obs.ah.a)
  
  boo.ah.h <- as.numeric(names(obs.ah.h))
  boo.ah.a <- as.numeric(names(obs.ah.a))
  boo.ah <- c(boo.ah.h, boo.ah.a)
  
  modelTemp.1x2 <- lm(obs.1x2 ~ boo.1x2)
  modelTemp.uo <- lm(obs.uo ~ boo.uo)
  modelTemp.ah <- lm(obs.ah ~ boo.ah)
  
  rsqu.season.1x2 <- c(rsqu.season.1x2, round(summary(modelTemp.1x2)$r.squared, 5))
  rmse.season.1x2 <- c(rmse.season.1x2, round(sqrt(mean(modelTemp.1x2$residuals^2)), 5))
  rsqu.season.uo <- c(rsqu.season.uo, round(summary(modelTemp.uo)$r.squared, 5))
  rmse.season.uo <- c(rmse.season.uo, round(sqrt(mean(modelTemp.uo$residuals^2)), 5))
  rsqu.season.ah <- c(rsqu.season.ah, round(summary(modelTemp.ah)$r.squared, 5))
  rmse.season.ah <- c(rmse.season.ah, round(sqrt(mean(modelTemp.ah$residuals^2)), 5))
  
  p1.temp <- exp(1/(nrow(dataTemp)) * sum(dataTemp$LogCorrect1x2))
  p2.temp <- 1/(nrow(dataTemp))*sum((1 - dataTemp$Correct1X2)**2 + (dataTemp$IncorrectA1X2)**2 + (dataTemp$IncorrectB1X2)**2 )
  p1.season.1x2 <- c(p1.season.1x2, round(p1.temp, 5))
  p2.season.1x2 <- c(p2.season.1x2, round(p2.temp ,5))
  
  p1.uo.temp <- exp(1/(nrow(dataTemp)) * sum(dataTemp$LogCorrectUO))
  p2.uo.temp <- 1/(nrow(dataTemp))*sum((1 - dataTemp$CorrectUO)**2 + (dataTemp$IncorrectUO)**2)
  p1.season.uo<- c(p1.season.uo, round(p1.uo.temp, 5))
  p2.season.uo <- c(p2.season.uo, round(p2.uo.temp, 5))
  
  dataTempAH <- ensco.ah.results[ensco.ah.results$Season == i, ]
  
  p1.ah.temp <- exp(1/(nrow(dataTempAH)) * sum(dataTempAH$LogCorrectAH))
  p2.ah.temp <- 1/(nrow(dataTempAH)) * sum((1 - dataTempAH$CorrectAH)**2 + (dataTempAH$IncorrectAH)**2)
  p1.season.ah <- c(p1.season.ah, round(p1.ah.temp, 5))
  p2.season.ah <- c(p2.season.ah, round(p2.ah.temp, 5))
  
  slope.season.1x2 <- c(slope.season.1x2, modelTemp.1x2$coefficients[2]) #Coefficients[2] is the gradient
  slope.season.uo <- c(slope.season.uo, modelTemp.uo$coefficients[2])
  slope.season.ah <- c(slope.season.ah, modelTemp.ah$coefficients[2])
}
rmse.season <- matrix( c(rmse.season.1x2, rmse.season.uo, rmse.season.ah), ncol=3, byrow=F, dimnames = list(seasons, c("1x2", "UO", "AH")))
rsqu.season <- matrix( c(rsqu.season.1x2, rsqu.season.uo, rsqu.season.ah), ncol=3, byrow=F, dimnames = list(seasons, c("1x2", "UO", "AH")))
p.values.season <- matrix(c(p1.season.1x2, p2.season.1x2, p1.season.uo, p2.season.uo, p1.season.ah, p2.season.ah), ncol=15, byrow=T, dimnames = list(c("P1 1X2", "P2 1X2", "P1 UO", "P2 UO", "P1 AH", "P2 AH"), seasons))
slope.season <- matrix(c(slope.season.1x2, slope.season.uo, slope.season.ah), ncol = 3, byrow = F, dimnames = list(seasons, c("1X2", "UO", "AH")))

#To present these in the text, we use the following two matrices (lots of values => split over 2)
accuracyvaluematrix.byseason1 <- round(matrix(c(rsqu.season[,1], rmse.season[,1], p1.season, p2.season, slope.season[,1], rsqu.season[,2], rmse.season[,2], p1.season.uo, p2.season.uo, slope.season[,2]), nrow = 15, byrow = F), 4)
rownames(accuracyvaluematrix.byseason1) <- seasons
accuracyvaluematrix.byseason2 <- round(matrix(c(rsqu.season[,3], rmse.season[,3], p1.season.ah, p2.season.ah, slope.season[,3]), nrow = 15, byrow = F), 4)
rownames(accuracyvaluematrix.byseason2) <- seasons



#Plotting these values -- We make 15 plots and arrange onto one figure after:
rsqu.season.plot.1x2 <- ggplot(NULL, aes(y=rsqu.season.1x2, x=c(2005:2019))) + geom_jitter(color = "violetred1") + theme_light() + labs(x = 'Year', y = 'R2', title = 'R2, 1X2') + geom_smooth(method = 'lm', se = F, color = "violetred1")

rmse.season.plot.1x2 <- ggplot(NULL, aes(y=rmse.season.1x2, x=c(2005:2019))) + geom_jitter(color = "violetred1") + theme_light() + labs(x = 'Year', y = 'RMSE', title = 'RMSE, 1X2') + geom_smooth(method = 'lm', se = F, color = "violetred1")

p1.season.plot.1x2 <- ggplot(NULL, aes(y=p1.season.1x2, x=c(2005:2019))) + geom_jitter(color = "violetred1") + theme_light() + labs(x = 'Year', y = 'P1', title = 'P1, 1X2') + geom_smooth(method = 'lm', se = F, color = "violetred1")

p2.season.plot.1x2 <- ggplot(NULL, aes(y=p2.season.1x2, x=c(2005:2019))) + geom_jitter(color = "violetred1") + theme_light() + labs(x = 'Year', y = 'P2', title = 'P2, 1X2') + geom_smooth(method = 'lm', se = F, color = "violetred1")

slope.season.plot.1x2 <- ggplot(NULL, aes(y=slope.season.1x2, x=c(2005:2019))) + geom_jitter(color = "violetred1") + theme_light() + labs(x = 'Year', y = 'Slope', title = 'Slope, 1X2') + geom_smooth(method = 'lm', se = F, color = "violetred1") + geom_abline(slope = 0, intercept = 1, color = "black")

rsqu.season.plot.uo <- ggplot(NULL, aes(y=rsqu.season.uo, x=c(2005:2019))) + geom_jitter(color = "dodgerblue4") + theme_light() + labs(x = 'Year', y = 'R2', title = 'R2, UO') + geom_smooth(method = 'lm', se = F, color = "dodgerblue4")

rmse.season.plot.uo <- ggplot(NULL, aes(y=rmse.season.uo, x=c(2005:2019))) + geom_jitter(color = "dodgerblue4") + theme_light() + labs(x = 'Year', y = 'RMSE', title = 'RMSE, UO') + geom_smooth(method = 'lm', se = F, color = "dodgerblue4")

p1.season.plot.uo <- ggplot(NULL, aes(y=p1.season.uo, x=c(2005:2019))) + geom_jitter(color = "dodgerblue4") + theme_light() + labs(x = 'Year', y = 'P1', title = 'P1, UO') + geom_smooth(method = 'lm', se = F, color = "dodgerblue4")

p2.season.plot.uo <- ggplot(NULL, aes(y=p2.season.uo, x=c(2005:2019))) + geom_jitter(color = "dodgerblue4") + theme_light() + labs(x = 'Year', y = 'P2', title = 'P2, UO') + geom_smooth(method = 'lm', se = F, color = "dodgerblue4")

slope.season.plot.uo <- ggplot(NULL, aes(y=slope.season.uo, x=c(2005:2019))) + geom_jitter(color = "dodgerblue4") + theme_light() + labs(x = 'Year', y = 'Slope', title = 'Slope, UO') + geom_smooth(method = 'lm', se = F, color = "dodgerblue4") + geom_abline(slope = 0, intercept = 1, color = "black")

rsqu.season.plot.ah <- ggplot(NULL, aes(y=rsqu.season.ah, x=c(2005:2019))) + geom_jitter(color = "seagreen4") + theme_light() + labs(x = 'Year', y = 'R2', title = 'R2, AH') + geom_smooth(method = 'lm', se = F, color = "seagreen4")

rmse.season.plot.ah <- ggplot(NULL, aes(y=rmse.season.ah, x=c(2005:2019))) + geom_jitter(color = "seagreen4") + theme_light() + labs(x = 'Year', y = 'RMSE', title = 'RMSE, AH') + geom_smooth(method = 'lm', se = F, color = "seagreen4")

p1.season.plot.ah <- ggplot(NULL, aes(y=p1.season.ah, x=c(2005:2019))) + geom_jitter(color = "seagreen4") + theme_light() + labs(x = 'Year', y = 'P1', title = 'P1, AH') + geom_smooth(method = 'lm', se = F, color = "seagreen4")

p2.season.plot.ah <- ggplot(NULL, aes(y=p2.season.ah, x=c(2005:2019))) + geom_jitter(color = "seagreen4") + theme_light() + labs(x = 'Year', y = 'P2', title = 'P2, AH') + geom_smooth(method = 'lm', se = F, color = "seagreen4")

slope.season.plot.ah <- ggplot(NULL, aes(y=slope.season.ah, x=c(2005:2019))) + geom_jitter(color = "seagreen4") + theme_light() + labs(x = 'Year', y = 'Slope', title = 'Slope, AH') + geom_smooth(method = 'lm', se = F, color = "seagreen4") + geom_abline(slope = 0, intercept = 1, color = "black")

seasontimeplot <- grid.arrange(rsqu.season.plot.1x2, rsqu.season.plot.uo, rsqu.season.plot.ah, 
                               rmse.season.plot.1x2, rmse.season.plot.uo, rmse.season.plot.ah,
                               p1.season.plot.1x2,   p1.season.plot.uo,   p1.season.plot.ah, 
                               p2.season.plot.1x2,   p2.season.plot.uo,   p2.season.plot.ah, 
                               slope.season.plot.1x2, slope.season.plot.uo, slope.season.plot.ah, 
                               nrow = 5, top = 'English & Scottish Leagues Accuracy Statistics over Time')
ggsave(path="./writeup/images", filename="ensco_20_seasontimeplots.png", plot=seasontimeplot, unit="cm", width=20, height=25)


### OVERROUND ----
#Plots
or.ot.l <- ggplot(ensco, aes(x=AvgHProb, y=OneXTwoOverround, color = Level)) + geom_jitter(alpha = 0.5) + theme_light() + guides(col = guide_legend(ncol = 3)) + labs(x = "Consensus P(Home Win)", y = "Sum of Probabilities (1X2)", title = "Consensus P(Home Win) v. Bookmaker Commission, by Level", caption = "English/Scottish Leagues, 2005-20") + coord_cartesian(ylim = c(1, 1.3))

or.ot.s <- ggplot(ensco, aes(x=AvgHProb, y=OneXTwoOverround, color = Season)) + geom_jitter(alpha = 0.5) + theme_light() + guides(col = guide_legend(ncol = 3)) + labs(x = "Consensus P(Home Win)", y = "Sum of Probabilities (1X2)", title = "Consensus P(Home Win) v. Bookmaker Commission, by Season", caption = "English/Scottish Leagues, 2005-20") + coord_cartesian(ylim = c(1, 1.3))

or.uo.l <- ggplot(ensco,aes(x=Over2.5Prob,y=UnderOverOverround,color=Level)) + geom_jitter(alpha = 0.5) + theme_light() + guides(col = guide_legend(ncol = 3)) + labs(x = "Consensus P(Over 2.5 Goals)", y="Sum of Probabilities (UO)", title = "Consensus P(Over 2.5 Goals) v. Bookmaker Commission, by Level", caption = "English/Scottish Leagues, 2005-20") + coord_cartesian(ylim = c(1, 1.3))

or.uo.s <- ggplot(ensco,aes(x=Over2.5Prob,y=UnderOverOverround,color=Season)) + geom_jitter(alpha = 0.5) + theme_light() + guides(col = guide_legend(ncol = 3)) + labs(x = "Consensus P(Over 2.5 Goals)", y="Sum of Probabilities (UO)", title = "Consensus P(Over 2.5 Goals) v. Bookmaker Commission, by Season", caption = "English/Scottish Leagues, 2005-20") + coord_cartesian(ylim = c(1, 1.3))

or.ah.l <- ggplot(ensco, aes(x=AH.HProb, y=AHOverround, color = Level)) + geom_jitter(alpha = 0.5) + theme_light() + guides(col = guide_legend(ncol = 3)) + labs(x = "Consensus P(AH Home Win)", y = "Sum of Probabilities (AH)", title = "Consensus P(Home Win, AH) v. Bookmaker Commission, by Level", caption = "English/Scottish Leagues, 2005-20") + coord_cartesian(ylim = c(1, 1.3))

or.ah.s <- ggplot(ensco, aes(x=AH.HProb, y=AHOverround, color = Season)) + geom_jitter(alpha = 0.5) + theme_light() + guides(col = guide_legend(ncol = 3)) + labs(x = "Consensus P(AH Home Win)", y = "Sum of Probabilities (AH)", title = "Consensus P(Home Win, AH) v. Bookmaker Commission, by Season", caption = "English/Scottish Leagues, 2005-20") + coord_cartesian(ylim = c(1, 1.3))

ggsave(path = "./writeup/images", filename = "ensco_14a_overround_ot_l.png", plot=or.ot.l, unit="cm", width=20, height=10)
ggsave(path = "./writeup/images", filename = "ensco_14b_overround_ot_s.png", plot=or.ot.s, unit="cm", width=20, height=10)
ggsave(path = "./writeup/images", filename = "ensco_14c_overround_uo_l.png", plot=or.uo.l, unit="cm", width=20, height=10)
ggsave(path = "./writeup/images", filename = "ensco_14d_overround_uo_s.png", plot=or.uo.s, unit="cm", width=20, height=10)
ggsave(path = "./writeup/images", filename = "ensco_14e_overround_ah_l.png", plot=or.ah.l, unit="cm", width=20, height=10)
ggsave(path = "./writeup/images", filename = "ensco_14f_overround_ah_s.png", plot=or.ah.s, unit="cm", width=20, height=10)


#Overround Calcs
#Overall, across all leagues/season:
mean(ensco$OneXTwoOverround)
mean(ensco$UnderOverOverround)
mean(ensco$AHOverround)

#By Level
for (i in levels){
  print(paste("Level",i,"---------------------------------------------")) 
  print(paste("1X2 Overround: ",mean(ensco[ensco$Level==i,]$OneXTwoOverround)))
  print(paste("UO Overround: ",mean(ensco[ensco$Level==i,]$UnderOverOverround)))
  print(paste("AH Overround: ",mean(ensco[ensco$Level==i,]$AHOverround)))
}
#Select seasons (05/06, 12/13 and 19/20) (equally spaced)
for (j in c("0506", "1213", "1920")){
  print(paste("Season",j,"--------------------------------------------")) 
  print(paste("1X2 Overround: ",mean(ensco[ensco$Season==j,]$OneXTwoOverround)))
  print(paste("UO Overround: ",mean(ensco[ensco$Season==j,]$UnderOverOverround)))
  print(paste("AH Overround: ",mean(ensco[ensco$Season==j,]$AHOverround)))
}

#- End - 