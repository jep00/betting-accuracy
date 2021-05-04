#### (3) USING OUR RESULTS FOR A BETTING ALGORITHM 
library(ggplot2); library(scales); library(e1071)

## CLEANING AND OBTAINING DATA ----
# First, we need to trim the datasets to just the columns we need and obtain the Asian Handicap odds for elite leagues from F-D.co.uk

matchesTemp <- NULL; matches <- NULL
seasons <- c("0506", "0607", "0708", "0809", "0910", "1011", "1112", "1213", "1314", "1415", "1516", "1617", "1718", "1819", "1920")
divisions <- c("E0", "E1", "E2", "E3", "EC", "SC0", "SC1", "SC2", "SC3", "D1", "SP1", "F1", "I1", "P1")

for (i in seasons){
  for (j in divisions){
    matchesTemp <- read.csv(paste0("https://www.football-data.co.uk/mmz4281/", i, "/", j, ".csv"), fileEncoding="latin1")
    #The above line will download and read the .csv file in one go. 
    matchesTemp$Season <- with(matchesTemp,i)
    matchesTemp$Div <- with(matchesTemp, j)
    if (i=="1920"){
      matchesTemp$BbAvH <- matchesTemp$AvgH
      matchesTemp$BbAvA <- matchesTemp$AvgA
      matchesTemp$BbAvD <- matchesTemp$AvgD
      matchesTemp$BbAvAHH <- matchesTemp$AvgAHH
      matchesTemp$BbAvAHA <- matchesTemp$AvgAHA
      matchesTemp$BbAHh <- matchesTemp$AHh}
    else{}
    matchesTemp$HomeHandicap <- matchesTemp$BbAHh
    matchesTemp <- matchesTemp[,c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "BbAvH", "BbAvD", "BbAvA", "HomeHandicap", "BbAvAHH", "BbAvAHA", "Season")]
    matches <- rbind(matches, matchesTemp)
  }
}
matches <- na.omit(matches)

# Finding underlying probabilities of each event:-
matches$OT.HProb.PN <- with(matches, 1/(BbAvH))
matches$OT.DProb.PN <- with(matches, 1/(BbAvD))
matches$OT.AProb.PN <- with(matches, 1/(BbAvA))
matches$AH.HProb.PN <- with(matches, 1/(BbAvAHH))
matches$AH.AProb.PN <- with(matches, 1/(BbAvAHA))

# Finding consensus probabilities of each event:-
matches$OT.HProb <- with(matches, round(OT.HProb.PN / (OT.HProb.PN + OT.DProb.PN + OT.AProb.PN), 4))
matches$OT.AProb <- with(matches, round(OT.AProb.PN / (OT.HProb.PN + OT.DProb.PN + OT.AProb.PN), 4))
matches$AH.HProb <- with(matches,round(AH.HProb.PN / (AH.HProb.PN + AH.AProb.PN), 4))
matches$AH.AProb <- with(matches,round(AH.AProb.PN / (AH.HProb.PN + AH.AProb.PN), 4))

N = nrow(matches)

matches$FTHG.ah <- with(matches, rep(0,N))
for (m in 1:N){matches$FTHG.ah[m] <- matches$FTHG[m] + matches$HomeHandicap[m]}
matches$ah.gap <- with(matches, FTHG.ah - FTAG); matches$ah.res <- NULL
for (n in 1:N){
  if (matches$ah.gap[n]<(-0.25)){matches$ah.res[n] <- "aw"}
  else if (matches$ah.gap[n]==(-0.25)){matches$ah.res[n] <- "hfaw"}
  else if (matches$ah.gap[n]==0){matches$ah.res[n] <- "vo"}
  else if (matches$ah.gap[n]==0.25){matches$ah.res[n] <- "hfhm"}
  else if (matches$ah.gap[n]>0.25){matches$ah.res[n] <- "hm"}
  else{}
}


## PLACING BETS ----
# To assess our algorithm, we assume we use the first year (05/06) to obtain data (that is, the means and std devs) and place bets on any game after.
matches$OTHomeBet <- with(matches, 0); matches$OTAwayBet <- with(matches, 0)
matches$AHHomeBet <- with(matches, 0); matches$AHAwayBet <- with(matches, 0)

#Initial Bounds :-
matches0506 <- matches[matches$Season == '0506',]
mu.oth <- mean(matches0506$OT.HProb); sd.oth <- sd(matches0506$OT.HProb)
mu.ota <- mean(matches0506$OT.AProb); sd.ota <- sd(matches0506$OT.AProb)
mu.ahh <- mean(matches0506$AH.HProb); sd.ahh <- sd(matches0506$AH.HProb)
mu.aha <- mean(matches0506$AH.AProb); sd.aha <- sd(matches0506$AH.AProb)
n <- nrow(matches[matches$Season == "0506",]); N <- nrow(matches)
#Placing Bets:-
for (i in n:N){
  #Update the mean and std dev's with our new information
  mu.oth <- mean(matches$OT.HProb[1:i]); sd.oth <- sd(matches$OT.HProb[1:i])
  mu.ota <- mean(matches$OT.AProb[1:i]); sd.ota <- sd(matches$OT.AProb[1:i])
  mu.ahh <- mean(matches$AH.HProb[1:i]); sd.ahh <- sd(matches$AH.HProb[1:i])
  mu.aha <- mean(matches$AH.AProb[1:i]); sd.aha <- sd(matches$AH.AProb[1:i])
  #Do we bet on Home Win (1X2)?
  if (matches$OT.HProb[i] > mu.oth + 0.5*sd.oth){
    if (matches$OT.HProb[i] <= mu.oth + sd.oth){matches$OTHomeBet[i] <- 1}
    else if (matches$OT.HProb[i] > mu.oth + sd.oth & matches$OT.HProb[i] <= mu.oth + 1.5*sd.oth){matches$OTHomeBet[i] <- 2}
    else {matches$OTHomeBet[i] <- 3}}
  else {matches$OTHomeBet[i] <- 0}
  #Do we bet on Away Win (1X2)?
  if (matches$OT.AProb[i] > mu.ota + 0.5*sd.ota){
    if (matches$OT.AProb[i] <= mu.ota + sd.ota){matches$OTAwayBet[i] <- 1}
    else if (matches$OT.AProb[i] > mu.ota + sd.ota & matches$OT.AProb[i] <= mu.ota + 1.5*sd.ota){matches$OTAwayBet[i] <- 2}
    else {matches$OTAwayBet[i] <- 3}}
  else {matches$OTAwayBet[i] <- 0}
  #Do we bet on Home Win (AH)?
  if (matches$AH.HProb[i] > mu.ahh + 0.5*sd.ahh){
    if (matches$AH.HProb[i] <= mu.ahh + sd.ahh){matches$AHHomeBet[i] <- 1}
    else if (matches$AH.HProb[i] > mu.ahh + sd.ahh & matches$AH.HProb[i] <= mu.ahh + 1.5*sd.ahh){matches$AHHomeBet[i] <- 2}
    else {matches$AHHomeBet[i] <- 3}}
  else {matches$AHHomeBet[i] <- 0}
  #Do we bet on Away Win (AH)?
  if (matches$AH.AProb[i] > mu.aha + 0.5*sd.aha){
    if (matches$AH.AProb[i] <= mu.aha + sd.aha){matches$AHAwayBet[i] <- 1}
    else if (matches$AH.AProb[i] > mu.aha + sd.aha & matches$AH.AProb[i] <= mu.aha + 1.5*sd.aha){matches$AHAwayBet[i] <- 2}
    else {matches$AHAwayBet[i] <- 3}}
  else {matches$AHAwayBet[i] <- 0}
}

## FINDING RESULTS ----
matches$OTHReturns <- with(matches, 0); matches$OTAReturns <- with(matches, 0)
matches$AHHReturns <- with(matches, 0); matches$AHAReturns <- with(matches, 0)

for (i in n:N){
  if (matches$FTR[i]=="H"){
    matches$OTHReturns[i] <- (matches$BbAvH[i] - 1) * matches$OTHomeBet[i]
    matches$OTAReturns[i] <- -matches$OTAwayBet[i]}
  else if (matches$FTR[i]=="A"){
    matches$OTAReturns[i] <- (matches$BbAvA[i] - 1) * matches$OTAwayBet[i]
    matches$OTHReturns[i] <- -matches$OTHomeBet[i]}
  else {
    matches$OTHReturns[i] <- -matches$OTHomeBet[i]
    matches$OTAReturns[i] <- -matches$OTAwayBet[i]}
}

for (i in n:N){
  if (matches$ah.res[i]=="aw"){
    matches$AHAReturns[i] <- (matches$BbAvAHA[i] - 1) * matches$AHAwayBet[i]
    matches$AHHReturns[i] <- -matches$AHHomeBet[i]
  }
  else if (matches$ah.res[i]=="hfaw"){
    matches$AHAReturns[i] <- (matches$BbAvAHA[i] - 1) * 0.5 * matches$AHAwayBet[i] - 0.5 * matches$AHAwayBet[i]
    matches$AHHReturns[i] <- -matches$AHHomeBet[i]
  }
  else if (matches$ah.res[i]=="hm"){
    matches$AHHReturns[i] <- (matches$BbAvAHH[i] - 1) * matches$AHHomeBet[i]
    matches$AHAReturns[i] <- -matches$AHAwayBet[i]
  }
  else if (matches$ah.res[i]=="hfhm"){
    matches$AHHReturns[i] <- (matches$BbAvAHH[i] - 1) * 0.5 * matches$AHHomeBet[i] - 0.5 * matches$AHHomeBet[i]
    matches$AHAReturns[i] <- -matches$AHAwayBet[i]
  }
  else{ #i.e. Void
    matches$AHHReturns[i] <- 0
    matches$AHAReturns[i] <- 0
  }
}

#Cumulative Returns (for our plot):-
matches$C.OTHReturns <- with(matches, 0)
matches$C.OTAReturns <- with(matches, 0)
matches$C.AHHReturns <- with(matches, 0)
matches$C.AHAReturns <- with(matches, 0)
matches$C.Returns <- with(matches, 0)

matches$C.OTHReturns[1] <- matches$OTHReturns[1]
matches$C.OTAReturns[1] <- matches$OTAReturns[1]
matches$C.AHHReturns[1] <- matches$AHHReturns[1]
matches$C.AHAReturns[1] <- matches$AHAReturns[1]
matches$C.Returns[1] <- matches$OTHReturns[1] + matches$OTAReturns[1] + matches$AHHReturns[1] + matches$AHAReturns[1]

for (i in 2:N){matches$C.Returns[i] <- matches$C.Returns[i-1] + matches$OTHReturns[i] + matches$OTAReturns[i] + matches$AHHReturns[i] + matches$AHAReturns[i]}
for (i in 2:N){matches$C.OTHReturns[i] <- matches$C.OTHReturns[i-1] + matches$OTHReturns[i]}
for (i in 2:N){matches$C.OTAReturns[i] <- matches$C.OTAReturns[i-1] + matches$OTAReturns[i]}
for (i in 2:N){matches$C.AHHReturns[i] <- matches$C.AHHReturns[i-1] + matches$AHHReturns[i]}
for (i in 2:N){matches$C.AHAReturns[i] <- matches$C.AHAReturns[i-1] + matches$AHAReturns[i]}

matches$BetIndex <- with(matches,0)
for (i in n:N){matches$BetIndex[i] <- (i-(n-1))}

## PLOTS ----
cr1 <- ggplot(matches,aes(x=BetIndex,y=C.OTHReturns,color="1X2 H Returns")) + 
  geom_line() +
  geom_line(aes(x=BetIndex, y=C.OTAReturns, color="1X2 A Returns")) +
  geom_line(aes(x=BetIndex, y=C.AHHReturns, color="AH H Returns")) +
  geom_line(aes(x=BetIndex, y=C.AHAReturns, color="AH A Returns")) +
  #geom_line(aes(x=BetIndex, y=C.Returns, color="Total Returns"), linetype="dotted") +
  theme_light() + labs(title="Cumulative Winnings using our Algorithm", caption="All matches, 2006-20", x="Match Index", y="Returns (unit)") +
  scale_color_manual(name="Bet Type", values=c("1X2 H Returns" = "blue", "1X2 A Returns" = "coral", "AH H Returns" = "green", "AH A Returns" = "purple", "Total Returns" = "black")) + geom_hline(yintercept=0, color="black", linetype="dotted", alpha=.5)

cr2 <- ggplot(matches, aes(x=BetIndex, y=C.OTHReturns, color="1X2 H Returns")) + 
  geom_line() +
  geom_line(aes(x=BetIndex, y=C.OTAReturns, color="1X2 A Returns")) +
  geom_line(aes(x=BetIndex, y=C.AHHReturns, color="AH H Returns")) +
  geom_line(aes(x=BetIndex, y=C.AHAReturns, color="AH A Returns")) +
  geom_line(aes(x=BetIndex, y=C.Returns, color="Total Returns"), linetype="twodash") +
  scale_color_manual(name="Bet Type", values=c("1X2 H Returns" = "blue", "1X2 A Returns" = "coral", "AH H Returns" = "green", "AH A Returns" = "purple", "Total Returns" = "black")) +
  theme_light() + coord_cartesian(xlim=c(0, 100), ylim=c(-25,25)) + geom_hline(yintercept=0, color="black", linetype="dotted", alpha=.5) + labs(title="Cumulative Winnings (First 100 Games)\n using our Algorithm", caption="All matches from 2006-20", x="Match Index", y="Returns (unit)")

ggsave(path = "./writeup/images", filename = "model_01.png", plot=cr1, unit="cm", width=15, height=15)
ggsave(path = "./writeup/images", filename = "model_02.png", plot=cr2, unit="cm", width=15, height=10)

## ACCURACY OF THE ALGORITHM  ----
n.OTH <- nrow(matches[matches$OTHomeBet > 0,])
n.OTA <- nrow(matches[matches$OTAwayBet > 0,])
n.AHH <- nrow(matches[matches$AHHomeBet > 0,])
n.AHA <- nrow(matches[matches$AHAwayBet > 0,])
n.BetsPlaced <- n.OTH + n.OTA + n.AHH + n.AHA
n.MatchesBet <- nrow(matches[which(matches$OTHomeBet > 0 | matches$OTAwayBet > 0 | matches$AHHomeBet > 0 | matches$AHAwayBet > 0),])
n.BetsMatrix <- matrix(round(c(n.OTH, n.OTA, n.AHH, n.AHA, n.BetsPlaced), 5), ncol = 5)

accuracy.ot.h <- nrow(matches[matches$OTHReturns > 0,]) / n.OTH * 100
accuracy.ot.a <- nrow(matches[matches$OTAReturns > 0,]) / n.OTA * 100
accuracy.ah.h <- nrow(matches[matches$AHHReturns > 0,]) / n.AHH * 100
accuracy.ah.a <- nrow(matches[matches$AHAReturns > 0,]) / n.AHA * 100
accuracy.overall <- 100 * (nrow(matches[matches$OTHReturns > 0,]) + nrow(matches[matches$OTAReturns > 0,]) + nrow(matches[matches$AHHReturns > 0,]) + nrow(matches[matches$AHAReturns > 0,])) / n.BetsPlaced

accuracy <- matrix(c(accuracy.ot.h, accuracy.ot.a, accuracy.ah.h, accuracy.ah.a, accuracy.overall), ncol = 5)

total.winnings <- sum(matches$OTHReturns) + sum(matches$OTAReturns) + sum(matches$AHHReturns) + sum(matches$AHAReturns)

bet.winnings <- matrix(c(sum(matches$OTHReturns), sum(matches$OTAReturns), sum(matches$AHHReturns), sum(matches$AHAReturns), total.winnings), ncol = 5)

bet.analysis <- matrix(c(n.BetsMatrix, bet.winnings, accuracy), nrow = 5, byrow = F, dimnames = list(c('1x2 H', '1x2 A', 'AH H', 'AH A', 'Overall'), c('Bets Placed','Winnings', 'Accuracy (%)')))
bet.analysis

## IGNORING THE 'LOW' PERFORMING LEAGUES (ALTERNATIVE METHOD) ----
#From our analysis, whilst all leagues performed well, we noticed the German and French, and English/Scottish Level 2 Leagues.

#First, we copy the bets we placed earlier into new columns:
matches$OTHomeBet.alt <- with(matches, OTHomeBet)
matches$OTAwayBet.alt <- with(matches, OTAwayBet)
matches$AHHomeBet.alt <- with(matches, AHHomeBet)
matches$AHAwayBet.alt <- with(matches, AHAwayBet)

#Removing bets placed in France, Germany and Level 2, Eng/Sco:-
for (i in 1:N){
  if (matches$Div[i] %in% c("D1", "F1", "E2", "E3", "SC1")){
    matches$OTHomeBet.alt[i] <- 0
    matches$OTAwayBet.alt[i] <- 0
    matches$AHHomeBet.alt[i] <- 0
    matches$AHAwayBet.alt[i] <- 0
  }
}

#And doing the same with returns:-
matches$OTHRet.alt <- with(matches, OTHReturns)
matches$OTARet.alt <- with(matches, OTAReturns)
matches$AHHRet.alt <- with(matches, AHHReturns)
matches$AHARet.alt <- with(matches, AHAReturns)
for (i in 1:N){
  if (matches$Div[i] %in% c("D1", "F1", "E2", "E3", "SC1")){
    matches$OTHRet.alt[i] <- 0
    matches$OTARet.alt[i] <- 0
    matches$AHHRet.alt[i] <- 0
    matches$AHARet.alt[i] <- 0
  }
}
#Cumulative Returns
matches$C.OTHRet.alt <- with(matches,0); matches$C.OTARet.alt <- with(matches,0); matches$C.AHHRet.alt <- with(matches,0); matches$C.AHARet.alt <- with(matches,0); matches$C.Returns.alt <- with(matches, 0)

matches$C.OTHRet.alt[1] <- matches$OTHRet.alt[1]
matches$C.OTARet.alt[1] <- matches$OTARet.alt[1]
matches$C.AHHRet.alt[1] <- matches$AHHRet.alt[1]
matches$C.AHARet.alt[1] <- matches$AHARet.alt[1]
matches$C.Returns.alt[1] <- matches$OTHRet.alt[1] + matches$OTARet.alt[1] + matches$AHHRet.alt[1] + matches$AHARet.alt[1]

for (i in 2:N){matches$C.Returns.alt[i] <- matches$C.Returns.alt[i-1] + matches$OTHRet.alt[i] + matches$OTARet.alt[i] + matches$AHHRet.alt[i] + matches$AHARet.alt[i]}
for (i in 2:N){matches$C.OTHRet.alt[i] <- matches$C.OTHRet.alt[i-1] + matches$OTHRet.alt[i]}
for (i in 2:N){matches$C.OTARet.alt[i] <- matches$C.OTARet.alt[i-1] + matches$OTARet.alt[i]}
for (i in 2:N){matches$C.AHHRet.alt[i] <- matches$C.AHHRet.alt[i-1] + matches$AHHRet.alt[i]}
for (i in 2:N){matches$C.AHARet.alt[i] <- matches$C.AHARet.alt[i-1] + matches$AHARet.alt[i]}

#Accuracy
n.OTH.alt <- nrow(matches[matches$OTHomeBet.alt > 0,])
n.OTA.alt <- nrow(matches[matches$OTAwayBet.alt > 0,])
n.AHH.alt <- nrow(matches[matches$AHHomeBet.alt > 0,])
n.AHA.alt <- nrow(matches[matches$AHAwayBet.alt > 0,])
n.BetsP.alt <- n.OTH.alt + n.OTA.alt + n.AHH.alt + n.AHA.alt
n.BetsMatrix.alt <- matrix(round(c(n.OTH.alt, n.OTA.alt, n.AHH.alt, n.AHA.alt, n.BetsP.alt), 5), ncol = 5)

accalt.ot.h <- nrow(matches[matches$OTHRet.alt > 0,]) / n.OTH.alt * 100
accalt.ot.a <- nrow(matches[matches$OTARet.alt > 0,]) / n.OTA.alt * 100
accalt.ah.h <- nrow(matches[matches$AHHRet.alt > 0,]) / n.AHH.alt * 100
accalt.ah.a <- nrow(matches[matches$AHARet.alt > 0,]) / n.AHA.alt * 100

accalt.ovr <- 100 * (nrow(matches[matches$OTHRet.alt > 0,]) + nrow(matches[matches$OTARet.alt > 0,]) + nrow(matches[matches$AHHRet.alt > 0,]) + nrow(matches[matches$AHARet.alt > 0,])) / n.BetsP.alt
accalt <- matrix(c(accalt.ot.h, accalt.ot.a, accalt.ah.h, accalt.ah.a, accalt.ovr), ncol = 5)
total.wins.alt <- sum(matches$OTHRet.alt) + sum(matches$OTARet.alt) + sum(matches$AHHRet.alt) + sum(matches$AHARet.alt)
bet.wins.alt <- matrix(c(sum(matches$OTHRet.alt), sum(matches$OTARet.alt), sum(matches$AHHRet.alt), sum(matches$AHARet.alt), total.wins.alt), ncol = 5)
bet.analysis.alt <- matrix(c(n.BetsMatrix.alt, bet.wins.alt, accalt), nrow = 5, byrow = F, dimnames = list(c('1x2 H', '1x2 A', 'AH H', 'AH A', 'Overall'), c('Bets Placed','Winnings', 'Accuracy (%)')))

bet.analysis.alt

## COMPARISON OF ACCURACY AGAINST RANDOMLY PLACED BETS ----
#To compare our algorithm against randomly placed bets, we will choose a random subset of N matches and find the winnings.

nRunRBS <- 10

N <- nrow(matches)
for (i in 1:nRunRBS){
  #Reset probabilities for 1X2 Home
  p1 <- nrow(matches[matches$OTHomeBet == 1,]) / nrow(matches[(n+1):N,])
  p2 <- nrow(matches[matches$OTHomeBet == 2,]) / nrow(matches[(n+1):N,])
  p3 <- nrow(matches[matches$OTHomeBet == 3,]) / nrow(matches[(n+1):N,])
  p0 <- 1 - (p1 + p2 + p3)
  
  matches$rand.Bet.OTH <- with(matches, 0)
  matches$rand.Ret.OTH <- with(matches, 0)
  matches$rand.Bet.OTH <- with(matches, rand.Bet.OTH + rdiscrete(n = nrow(matches), values = 0:3, probs=c(p0, p1, p2, p3)))
  
  for(i in n:N){
    if (matches$FTR[i] == "H"){matches$rand.Ret.OTH[i] <- (matches$BbAvH[i]-1) * matches$rand.Bet.OTH[i]}
    else{matches$rand.Ret.OTH[i] <- -matches$rand.Bet.OTH[i]}}
  
  #Reset probabilities for 1X2 Away
  p1 <- nrow(matches[matches$OTAwayBet == 1,]) / nrow(matches[(n+1):N,])
  p2 <- nrow(matches[matches$OTAwayBet == 2,]) / nrow(matches[(n+1):N,])
  p3 <- nrow(matches[matches$OTAwayBet == 3,]) / nrow(matches[(n+1):N,])
  p0 <- 1 - (p1 + p2 + p3)
  
  matches$rand.Bet.OTA <- with(matches, 0)
  matches$rand.Ret.OTA <- with(matches, 0)
  matches$rand.Bet.OTA <- with(matches, rand.Bet.OTA + rdiscrete(n = nrow(matches), values = 0:3, probs=c(p0, p1, p2, p3)))
  
  for (i in n:N){
    if (matches$FTR[i] == "A"){matches$rand.Ret.OTA[i] <- (matches$BbAvA[i]-1) * matches$rand.Bet.OTA[i]}
    else{matches$rand.Ret.OTA[i] <- -matches$rand.Bet.OTA[i]}}
  
  #Reset probabilities for AH Home
  p1 <- nrow(matches[matches$AHHomeBet == 1,]) / nrow(matches[(n+1):N,])
  p2 <- nrow(matches[matches$AHHomeBet == 2,]) / nrow(matches[(n+1):N,])
  p3 <- nrow(matches[matches$AHHomeBet == 3,]) / nrow(matches[(n+1):N,])
  p0 <- 1 - (p1 + p2 + p3)
  
  matches$rand.Bet.AHH <- with(matches, 0)
  matches$rand.Ret.AHH <- with(matches, 0)
  matches$rand.Bet.AHH <- with(matches, rand.Bet.AHH + rdiscrete(n = nrow(matches), values = 0:3, probs=c(p0, p1, p2, p3)))
  for (i in n:N){
    if (matches$ah.res[i] == "hm"){matches$rand.Ret.AHH[i] <- (matches$BbAvAHH[i]-1) * matches$rand.Bet.AHH[i]}
    else if (matches$ah.res[i] == "hfhm"){matches$rand.Ret.AHH[i] <- (matches$BbAvAHH[i]-1) * 0.5 * matches$rand.Bet.AHH[i] - (0.5 * matches$rand.Bet.AHH[i])}
    else{matches$rand.Ret.AHH[i] <- -matches$rand.Bet.AHH[i]}}
  
  #Reset probabilities for AH Away
  p1 <- nrow(matches[matches$AHAwayBet == 1,]) / nrow(matches[(n+1):N,])
  p2 <- nrow(matches[matches$AHAwayBet == 2,]) / nrow(matches[(n+1):N,])
  p3 <- nrow(matches[matches$AHAwayBet == 3,]) / nrow(matches[(n+1):N,])
  p0 <- 1 - (p1 + p2 + p3)
  
  matches$rand.Bet.AHA <- with(matches, 0)
  matches$rand.Ret.AHA <- with(matches, 0)
  matches$rand.Bet.AHA <- with(matches, rand.Bet.AHA + rdiscrete(n = nrow(matches), values = 0:3, probs=c(p0, p1, p2, p3)))
  for (i in n:N){
    if (matches$ah.res[i] == "aw"){matches$rand.Ret.AHA[i] <- (matches$BbAvAHA[i]-1) * matches$rand.Bet.AHA[i]}
    else if (matches$ah.res[i] == "hfaw"){matches$rand.Ret.AHA[i] <- (matches$BbAvAHA[i]-1) * 0.5 * matches$rand.Bet.AHA[i] - (0.5 * matches$rand.Bet.AHA[i])}
    else{matches$rand.Ret.AHA[i] <- -matches$rand.Bet.AHA[i]}}
  
  #Creating our analysis matrix:
  #No. of bets
  #We add the caveat of BetIndex > 0 to avoid betting on the 05/06 season
  n.Ran.OTH <- nrow(matches[matches$rand.Bet.OTH > 0 & matches$BetIndex > 0,])
  n.Ran.OTA <- nrow(matches[matches$rand.Bet.OTA > 0 & matches$BetIndex > 0,])
  n.Ran.AHH <- nrow(matches[matches$rand.Bet.AHH > 0 & matches$BetIndex > 0,])
  n.Ran.AHA <- nrow(matches[matches$rand.Bet.AHA > 0 & matches$BetIndex > 0,])
  n.RanBets <- n.Ran.OTH + n.Ran.OTA + n.Ran.AHH + n.Ran.AHA
  n.RanMatx <- matrix(c(n.Ran.OTH, n.Ran.OTA, n.Ran.AHH, n.Ran.AHA, n.RanBets), ncol = 5)
  
  stake.OTH <- sum(matches$rand.Bet.OTH)
  stake.OTA <- sum(matches$rand.Bet.OTA)
  stake.AHH <- sum(matches$rand.Bet.AHH)
  stake.AHA <- sum(matches$rand.Bet.AHA)
  stake.ovr <- stake.OTH + stake.OTA + stake.AHH + stake.AHA
  stakeMatx <- matrix(c(stake.OTH, stake.OTA, stake.AHH, stake.AHA, stake.ovr), ncol = 5)
  
  #Accuracy percentage
  acc.Ran.OTH <- nrow(matches[matches$rand.Ret.OTH > 0,]) / n.Ran.OTH * 100
  acc.Ran.OTA <- nrow(matches[matches$rand.Ret.OTA > 0,]) / n.Ran.OTA * 100
  acc.Ran.AHH <- nrow(matches[matches$rand.Ret.AHH > 0,]) / n.Ran.AHH * 100
  acc.Ran.AHA <- nrow(matches[matches$rand.Ret.AHA > 0,]) / n.Ran.AHA * 100
  acc.Ran.ovr <- 100 * (nrow(matches[matches$rand.Ret.OTH > 0,]) + nrow(matches[matches$rand.Ret.OTA > 0,]) + nrow(matches[matches$rand.Ret.AHH > 0,]) + nrow(matches[matches$rand.Ret.AHA > 0,])) / n.RanBets
  acc.Ran <- matrix(c(acc.Ran.OTH, acc.Ran.OTA, acc.Ran.AHH, acc.Ran.AHA, acc.Ran.ovr), ncol = 5)
  
  #Winnings
  ran.winnings <- sum(matches$rand.Ret.OTH) + sum(matches$rand.Ret.OTA) + sum(matches$rand.Ret.AHH) + sum(matches$rand.Ret.AHA)
  ran.winningsMtx <- matrix(c(sum(matches$rand.Ret.OTH), sum(matches$rand.Ret.OTA), sum(matches$rand.Ret.AHH), sum(matches$rand.Ret.AHA), ran.winnings), ncol = 5)
  
  bet.analysis.random <- matrix(c(n.RanMatx, stakeMatx, ran.winningsMtx, acc.Ran), nrow = 5, byrow = F, dimnames = list(c('1x2 H', '1x2 A', 'AH H', 'AH A', 'Overall'), c('Bets Placed', 'Stake', 'Winnings', 'Accuracy (%)')))
  print(bet.analysis.random)
}

## PLOTTING OF RANDOM BET STRATEGY ----

#Run par(mfrow=c(2,2)) if you want all four plots in one graph. Else, it is recommended to run each plot separately.
par(mfrow = c(2,2)); nRuns <- 30; alpha0 <- 1/nRuns
#nRuns is the No. runs you wish to have. More = slower.

#1X2 Home
plot(matches$BetIndex, matches$C.OTHReturns, col = alpha("red"), type = 'l', ylim = c(-3000, 10), xlab = 'Index', ylab = 'Returns', main = '1X2 Home Win')
for (i in 1:nRuns){
  #Reset the random bet, 1X2 Home
  p1 <- nrow(matches[matches$OTHomeBet == 1,]) / nrow(matches[(n+1):N,])
  p2 <- nrow(matches[matches$OTHomeBet == 2,]) / nrow(matches[(n+1):N,])
  p3 <- nrow(matches[matches$OTHomeBet == 3,]) / nrow(matches[(n+1):N,])
  p0 <- 1 - (p1 + p2 + p3)
  
  matches$rand.Bet.OTH <- with(matches, 0)
  matches$rand.Ret.OTH <- with(matches, 0)
  matches$rand.Bet.OTH <- with(matches, rand.Bet.OTH + rdiscrete(n = nrow(matches), values = 0:3, probs=c(p0, p1, p2, p3)))
  
  #For the random bets, we have 'placed bets' on the 05/06 season, we will ignore it for the plots 
  for(i in n:N){
    if (matches$FTR[i] == "H"){matches$rand.Ret.OTH[i] <- (matches$BbAvH[i]-1) * matches$rand.Bet.OTH[i]}
      else{matches$rand.Ret.OTH[i] <- -matches$rand.Bet.OTH[i]}}
  
  matches$rand.CumR.OTH <- with(matches, 0)
  matches$rand.CumR.OTH[n] <- matches$rand.Ret.OTH[n]
  for (i in n:N){matches$rand.CumR.OTH[i] <- matches$rand.CumR.OTH[i-1] + matches$rand.Ret.OTH[i]}
  
  lines(matches$BetIndex, matches$rand.CumR.OTH, col = alpha("blue", alpha0), type = 'l')
}

lines(matches$BetIndex, matches$C.OTHReturns, col = alpha("red"), type = 'l')
lines(matches$BetIndex, matches$C.OTHRet.alt, col = alpha("green"), type = 'l')
legend(0, -2500, c('Our Method', 'Alternate', 'Random'), lty=c(1,1,1), col=c('red', 'green', 'blue'))

#1X2 Away
plot(matches$BetIndex, matches$C.OTAReturns, col = alpha("red"), type = 'l', ylim = c(-3000, 10), xlab = 'Index', ylab = 'Returns', main = '1X2 Away Win')
for (i in 1:nRuns){
  p1 <- nrow(matches[matches$OTAwayBet == 1,]) / nrow(matches[(n+1):N,])
  p2 <- nrow(matches[matches$OTAwayBet == 2,]) / nrow(matches[(n+1):N,])
  p3 <- nrow(matches[matches$OTAwayBet == 3,]) / nrow(matches[(n+1):N,])
  p0 <- 1 - (p1 + p2 + p3)
  
  #Reset the random bet, 1X2 Away
  matches$rand.Bet.OTA <- with(matches, 0)
  matches$rand.Ret.OTA <- with(matches, 0)
  matches$rand.Bet.OTA <- with(matches, rand.Bet.OTA + rdiscrete(n = nrow(matches), values = 0:3, probs=c(p0, p1, p2, p3)))
  
  for (i in n:N){
    if (matches$FTR[i] == "A"){matches$rand.Ret.OTA[i] <- (matches$BbAvA[i]-1) * matches$rand.Bet.OTA[i]}
    else{matches$rand.Ret.OTA[i] <- -matches$rand.Bet.OTA[i]}}
  
  matches$rand.CumR.OTA <- with(matches, 0)
  matches$rand.CumR.OTA[n] <- matches$rand.Ret.OTA[n]
  for (i in n:N){matches$rand.CumR.OTA[i] <- matches$rand.CumR.OTA[i-1] + matches$rand.Ret.OTA[i]}
  
  lines(matches$BetIndex, matches$rand.CumR.OTA, col=alpha("blue", alpha0), type='l')
}

lines(matches$BetIndex, matches$C.OTAReturns, col = alpha("red"), type = 'l')
lines(matches$BetIndex, matches$C.OTARet.alt, col = alpha("green"), type = 'l')
legend(0, -2500, c('Our Method', 'Alternate', 'Random'), lty=c(1,1,1), col=c('red', 'green', 'blue'))

#AH Home
plot(matches$BetIndex, matches$C.AHHReturns, col = alpha("red"), type = 'l', ylim = c(-3000, 10), xlab = 'Index', ylab = 'Returns', main = 'AH Home Win')
for (i in 1:nRuns){
  p1 <- nrow(matches[matches$AHHomeBet == 1,]) / nrow(matches[(n+1):N,])
  p2 <- nrow(matches[matches$AHHomeBet == 2,]) / nrow(matches[(n+1):N,])
  p3 <- nrow(matches[matches$AHHomeBet == 3,]) / nrow(matches[(n+1):N,])
  p0 <- 1 - (p1 + p2 + p3)
  
  #Reset the random bet, AH Home
  matches$rand.Bet.AHH <- with(matches, 0)
  matches$rand.Ret.AHH <- with(matches, 0)
  matches$rand.Bet.AHH <- with(matches, rand.Bet.AHH + rdiscrete(n = nrow(matches), values = 0:3, probs=c(p0, p1, p2, p3)))
  for (i in n:N){
    if (matches$ah.res[i] == "hm"){matches$rand.Ret.AHH[i] <- (matches$BbAvAHH[i]-1) * matches$rand.Bet.AHH[i]}
    else if (matches$ah.res[i] == "hfhm"){matches$rand.Ret.AHH[i] <- (matches$BbAvAHH[i]-1) * 0.5 * matches$rand.Bet.AHH[i] - (0.5 * matches$rand.Bet.AHH[i])}
    else{matches$rand.Ret.AHH[i] <- -matches$rand.Bet.AHH[i]}}
  
  matches$rand.CumR.AHH <- with(matches, 0)
  matches$rand.CumR.AHH[n] <- matches$rand.Ret.AHH[n]
  for (i in n:N){matches$rand.CumR.AHH[i] <- matches$rand.CumR.AHH[i-1] + matches$rand.Ret.AHH[i]}
  
  lines(matches$BetIndex, matches$rand.CumR.AHH, col=alpha("blue", alpha0), type='l')
}
lines(matches$BetIndex, matches$C.AHHReturns, col = alpha("red"), type = 'l')
lines(matches$BetIndex, matches$C.AHHRet.alt, col = alpha("green"), type = 'l')
legend(0, -2500, c('Our Method', 'Alternate', 'Random'), lty=c(1,1,1), col=c('red', 'green', 'blue'))

#AH Away
plot(matches$BetIndex, matches$C.AHAReturns, col = alpha("red"), type = 'l', ylim = c(-3000, 10), xlab = 'Index', ylab = 'Returns', main = 'AH Away Win')
for (i in 1:nRuns){
  p1 <- nrow(matches[matches$AHAwayBet == 1,]) / nrow(matches[(n+1):N,])
  p2 <- nrow(matches[matches$AHAwayBet == 2,]) / nrow(matches[(n+1):N,])
  p3 <- nrow(matches[matches$AHAwayBet == 3,]) / nrow(matches[(n+1):N,])
  p0 <- 1 - (p1 + p2 + p3)
  
  #Reset the random bet, AH Away
  matches$rand.Bet.AHA <- with(matches, 0)
  matches$rand.Ret.AHA <- with(matches, 0)
  matches$rand.Bet.AHA <- with(matches, rand.Bet.AHA + rdiscrete(n = nrow(matches), values = 0:3, probs=c(p0, p1, p2, p3)))
  for (i in n:N){
    if (matches$ah.res[i] == "aw"){matches$rand.Ret.AHA[i] <- (matches$BbAvAHA[i]-1) * matches$rand.Bet.AHA[i]}
    else if (matches$ah.res[i] == "hfaw"){matches$rand.Ret.AHA[i] <- (matches$BbAvAHA[i]-1) * 0.5 * matches$rand.Bet.AHA[i] - (0.5 * matches$rand.Bet.AHA[i])}
    else{matches$rand.Ret.AHA[i] <- -matches$rand.Bet.AHA[i]}}
  
  matches$rand.CumR.AHA <- with(matches, 0)
  matches$rand.CumR.AHA[n] <- matches$rand.Ret.AHA[n]
  for (i in n:N){matches$rand.CumR.AHA[i] <- matches$rand.CumR.AHA[i-1] + matches$rand.Ret.AHA[i]}
  
  lines(matches$BetIndex, matches$rand.CumR.AHA, col=alpha("blue", alpha0), type='l')
}
lines(matches$BetIndex, matches$C.AHAReturns, col = alpha("red"), type = 'l')
lines(matches$BetIndex, matches$C.AHARet.alt, col = alpha("green"), type = 'l')
legend(0, -2500, c('Our Method', 'Alternate', 'Random'), lty=c(1,1,1), col=c('red', 'green', 'blue'))


par(mfrow = c(1, 1)) #Reset graphical parameters
#- End -