#### (1) ANALYSING THE ACCURACY OF BETTING ON ELITE EUROPEAN LEAGUES
#Directory, Environment, Packages ----
setwd("~/Desktop/University/University Year 3/331MP/Data")
rm(list=ls())
#If not done before, install these:
#install.packages('car'); install.packages('MASS')
#install.packages('ggplot2'); #install.packages('gridExtra')
library(car); library(MASS); library(ggplot2); library(gridExtra)

### IDA; Analysis of one season, one league [chosen at random] ---- 
#Reading the data:
fr_l1_1617 <- read.csv("https://www.football-data.co.uk/mmz4281/1617/F1.csv")
fr_l1_1617 <- fr_l1_1617[,c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "BbAvH", "BbAvD", "BbAvA")]
fr_l1_1617 <- na.omit(fr_l1_1617)

#Finding consensus probabilities
#PN => Pre-Normalised (Underlying Probabilities)
fr_l1_1617$AvgHProbPN <- with(fr_l1_1617, round(1/BbAvH, 4))
fr_l1_1617$AvgDProbPN <- with(fr_l1_1617, round(1/BbAvD, 4))
fr_l1_1617$AvgAProbPN <- with(fr_l1_1617, round(1/BbAvA, 4))

fr_l1_1617$Overround <- with(fr_l1_1617, (AvgHProbPN + AvgDProbPN + AvgAProbPN))
fr_l1_1617$AvgHProb <- with(fr_l1_1617, round(AvgHProbPN/Overround,4))
fr_l1_1617$AvgDProb <- with(fr_l1_1617, round(AvgDProbPN/Overround,4))
fr_l1_1617$AvgAProb <- with(fr_l1_1617, round(AvgAProbPN/Overround,4))

#Simple calculations
basic.fr.ida <- matrix(c(mean(fr_l1_1617$AvgHProb), mean(fr_l1_1617$AvgDProb), mean(fr_l1_1617$AvgAProb), sd(fr_l1_1617$AvgHProb), sd(fr_l1_1617$AvgDProb), sd(fr_l1_1617$AvgAProb)), ncol=3, nrow=2, byrow=T)
rownames(basic.fr.ida) <- c('mean', 'sd')
colnames(basic.fr.ida) <- c('h', 'd', 'a')
basic.fr.ida;round(prop.table(table(fr_l1_1617$FTR)), 4)

##Histograms
idaHomeHIST <- ggplot(fr_l1_1617, aes(AvgHProb)) + geom_histogram(binwidth=0.05, fill="blue") + coord_cartesian(xlim=c(0,1)) + theme_light() + labs(title="Home Win", x=NULL, y=NULL)

idaAwayHIST <- ggplot(fr_l1_1617, aes(AvgAProb)) + geom_histogram(binwidth=0.05, fill="coral") + coord_cartesian(xlim=c(0,1)) + theme_light() + labs(title="Away Win", x=NULL, y=NULL)

idaDrawHIST <- ggplot(fr_l1_1617, aes(AvgDProb)) + geom_histogram(binwidth=0.05, fill="green4") + coord_cartesian(xlim=c(0,1)) + theme_light() + labs(title="Draw", x=NULL, y=NULL)

ida.histogram <- grid.arrange(idaHomeHIST, idaDrawHIST, idaAwayHIST, nrow=3, ncol=1,left="Frequency", bottom="Consensus Probability")

ggsave(path="./writeup/images", filename="elite_01_idahist.png", plot=ida.histogram, unit="cm", width=15, height=18)

### EDA ----
##Reading the data using a For Loop:
#Define which countries and seasons we need to read:
countries <- c("de", "en", "es", "fr", "it", "po")
co.we <- c("D1", "E0", "SP1", "F1", "I1", "P1") 
#n.b. The Premier League's code is 0; other countries are 1.
seasons <- c("0506", "0607", "0708", "0809", "0910", "1011", "1112", "1213", "1314", "1415", "1516", "1617", "1718", "1819", "1920")
eliteTemp <- NULL; elite <- NULL
for (i in seasons){
  for (j in 1:6){
    eliteTemp <- read.csv(paste0('https://www.football-data.co.uk/mmz4281/', i, '/', co.we[j],'.csv'), fileEncoding = 'latin1')
    eliteTemp$Country <- with(eliteTemp, countries[j])
    eliteTemp$Season <- with(eliteTemp, i)
    if (i=="1920"){
      eliteTemp$BbAvH<-eliteTemp$AvgH; eliteTemp$BbAvA<-eliteTemp$AvgA
      eliteTemp$BbAvD<-eliteTemp$AvgD
    }
    else{}
    eliteTemp <- eliteTemp[ ,c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "BbAvH", "BbAvD", "BbAvA", "Country", "Season")]
    elite <- rbind(elite, eliteTemp)
  }
}
elite <- na.omit(elite)

#Finding underlying probabilities: 
#Pre-Normalised Probabilities
elite$AvgHProbPN <- with(elite, round(1/BbAvH, 4)) 
elite$AvgDProbPN <- with(elite, round(1/BbAvD, 4))
elite$AvgAProbPN <- with(elite, round(1/BbAvA, 4))
#To normalise them:
elite$overround<-with(elite, (AvgHProbPN + AvgDProbPN + AvgAProbPN))
elite$AvgHProb <-with(elite, round(AvgHProbPN/overround, 4))
elite$AvgDProb <-with(elite, round(AvgDProbPN/overround, 4))
elite$AvgAProb <-with(elite, round(AvgAProbPN/overround, 4))

#For later analysis, we need the Correct/Incorrect Probabilities:
N<-nrow(elite); N
elite$Correct<-with(elite, rep(0, N))
elite$Incorr1<-with(elite, rep(0, N))
elite$Incorr2<-with(elite, rep(0, N))

for (i in 1:N){
  if ((elite$FTR[i])=="A"){
    elite$Correct[i]<-(elite$Correct[i] + elite$AvgAProb[i])
    elite$Incorr1[i]<-(elite$Incorr1[i] + elite$AvgDProb[i])
    elite$Incorr2[i]<-(elite$Incorr2[i] + elite$AvgHProb[i])}
  else if ((elite$FTR[i])=="H"){
    elite$Correct[i]<-(elite$Correct[i] + elite$AvgHProb[i])
    elite$Incorr1[i]<-(elite$Incorr1[i] + elite$AvgDProb[i])
    elite$Incorr2[i]<-(elite$Incorr2[i] + elite$AvgAProb[i])}
  else {
    elite$Correct[i]<-(elite$Correct[i] + elite$AvgDProb[i])
    elite$Incorr1[i]<-(elite$Incorr1[i] + elite$AvgAProb[i])
    elite$Incorr2[i]<-(elite$Incorr2[i] + elite$AvgHProb[i])} 
}
elite$logCorrect<-with(elite, rep(0,N))
for (j in 1:N){elite$logCorrect[j]<-log(elite$Correct[j], base=exp(1))}

## Simple calculations
basic.elite <- matrix(c(mean(elite$AvgHProb), mean(elite$AvgDProb), mean(elite$AvgAProb), sd(elite$AvgHProb), sd(elite$AvgDProb), sd(elite$AvgAProb)), ncol=3, nrow=2, byrow=T)
rownames(basic.elite) <- c('mean', 'sd')
colnames(basic.elite) <- c('h', 'd', 'a')
basic.elite; round(prop.table(table(elite$FTR)), 4) #Observed probabilities

### VISUAL ANALYSIS ----
## Boxplots
bp.home <- ggplot(elite, aes(x=FTR, y=AvgHProb)) + geom_boxplot(outlier.size=0.75, outlier.alpha=0.7, color="blue") + theme_light() + stat_boxplot(coef=5) + labs(x="Actual Result", y="Consensus Probability", title = "Home Win", caption = "") + coord_cartesian(ylim=c(0,1))

bp.draw <- ggplot(elite, aes(x=FTR, y=AvgDProb)) + geom_boxplot(outlier.size=0.75, outlier.alpha=0.7, color="green4") +theme_light() + stat_boxplot(coef=5) + labs(x="Actual Result", y="Consensus Probability", title = "Draw", caption = "") + coord_cartesian(ylim=c(0,1))

bp.away <- ggplot(elite, aes(x=FTR, y=AvgAProb)) + geom_boxplot(outlier.size=0.75, outlier.alpha=0.7, color="coral") + theme_light() + stat_boxplot(coef=5) + labs(x="Actual Result", y="Consensus Probability", title = "Away Win", caption="Elite European Leagues, 2005-2020") + coord_cartesian(ylim=c(0,1))

eda.bp.all <- grid.arrange(bp.home, bp.draw, bp.away, nrow=1, ncol=3)
ggsave(path="./writeup/images", filename="elite_02_boxplot.png", plot=eda.bp.all, unit="cm", width=15, height=10)

##Density Plots
eda.wins.dens.all <- ggplot(elite, aes(x=AvgHProb, color="HW")) + geom_density() + geom_density(data=elite, mapping=aes(x=AvgAProb, color="AW"), show.legend=T) + coord_cartesian(xlim=c(0,1)) + labs(title="Home and Away Wins", caption="Elite Leagues, 2005-2020", x="Consensus Probability", y="Density") + theme_light() + scale_color_manual(name="Market", values=c("HW" = "blue", "AW" = "coral"))

eda.draw.dens.all <- ggplot(elite, aes(x=AvgDProb, color="D")) + geom_density() + coord_cartesian(xlim=c(0,1)) + labs(title="Draws", caption="", x="Consensus Probability",y="Density") + theme_light() + scale_color_manual(name = "Market", values=c("D" = "green4"))

eda.density.all <- grid.arrange(eda.wins.dens.all, eda.draw.dens.all, nrow=1, ncol=2, left="", bottom="")
ggsave(path="./writeup/images", filename="elite_02_edadensall.png", plot=eda.density.all, unit="cm", width=15, height=7)
  
View(elite[elite$AvgDProb > 0.6,])  #Extremely high P(Draw)
View(elite[elite$AvgDProb > 0.35,]) #Unusually high P(Draw)

#Splitting these by league:
#Home Wins
eda.home.dens <- ggplot(elite, aes(x=AvgHProb, color=Country)) + geom_density() + coord_cartesian(xlim=c(0,1)) + labs(title="Home Win", x=NULL, y=NULL) + theme_light() + geom_vline(aes(xintercept=mean(AvgHProb)), linetype="dashed", size=0.3)+guides(y="none") + theme(legend.position="none")
#Away Wins
eda.away.dens <- ggplot(elite, aes(x=AvgAProb, color=Country)) + geom_density() + coord_cartesian(xlim=c(0,1)) +labs(title="Away Win", caption = "Elite European Leagues, 2005-20", x=NULL, y=NULL) + theme_light() + geom_vline(aes(xintercept=mean(AvgAProb)), linetype="dashed", size=0.3)+ guides(y="none") + theme(legend.position="none")
#Draws
eda.draw.dens <- ggplot(elite, aes(x=AvgDProb, color=Country)) + geom_density() + coord_cartesian(xlim=c(0,0.8)) + labs(title="Draw", x=NULL, y=NULL) + geom_vline(aes(xintercept=mean(AvgDProb)), linetype="dashed", size=0.3) + guides(y="none") + theme_light() + scale_colour_discrete(labels = c("Germany", "England", "Spain", "France", "Italy", "Portugal"))

eda.density <- grid.arrange(eda.home.dens, eda.draw.dens, eda.away.dens, nrow=3, ncol=1, left="", bottom="Consensus Probability")
ggsave(path="./writeup/images", filename="elite_02_edadens.png", plot=eda.density, unit="cm", width=15, height=18)

##Tile Plot
#We will group 5+ goals together
elite$FTHG.Tile <- with(elite,rep(0,N))
elite$FTAG.Tile <- with(elite,rep(0,N))
for (k in 1:N){
  if ((elite$FTHG[k])>=5){elite$FTHG.Tile[k] <- 5}
  else{elite$FTHG.Tile[k] <- elite$FTHG[k]}}
for (k in 1:N){
  if ((elite$FTAG[k])>=5){elite$FTAG.Tile[k] <- 5}
  else{elite$FTAG.Tile[k] <- elite$FTAG[k]}}
    
elite.tile <- ggplot(elite, aes(y=FTAG.Tile, x=FTHG.Tile)) + geom_tile(aes(fill = Correct)) + scale_fill_distiller(palette = "Greens", direction = 1, name="Correct\nProbability") + theme_light() + labs(title="Match Result v. Correct Consensus Probability", x="Home Goals Scored", y="Away Goals Scored", caption="Elite European Leagues, 2005-2020") + scale_y_discrete(limits=factor(c(1:4, "5+"))) +scale_x_discrete(limits=factor(c(1:4, "5+"))) +geom_abline(intercept=0, slope=1) + coord_cartesian(xlim=c(0,5), ylim=c(0,5))

ggsave(path="./writeup/images", filename="elite_05_tile.png", plot=elite.tile, unit="cm", width=15, height=15)
tpbinsizes.elite <- table(elite$FTAG.Tile, elite$FTHG.Tile) #Bin sizes
    
### CORRELATION ANALYSIS ---- 

## Binning the data:-
#Home Wins:-
elite$AvgHProb.cut <- cut(elite$AvgHProb, 124, include.lowest=T)
#First, we cut the data into 'bins' choosing 124 breaks
levels(elite$AvgHProb.cut) <- tapply(elite$AvgHProb, elite$AvgHProb.cut, mean)
#Tapply finds the mean of the bin, rather than taking the midpoint
elite.observed.probabilites.TabH <- prop.table(table(elite$FTR, elite$AvgHProb.cut), 2)[c(1, 2, 3),]
#The c(1,2,3) will remove any extra (blank) rows
elite.observed.probabilites.H <- elite.observed.probabilites.TabH[3,] 
#[n,]; if n = : 1 Away; 2 Draw; 3 Home (alphabetic)
elite.bookmaker.probabilites.H <- as.numeric(names(elite.observed.probabilites.H))

#Away Wins:-
elite$AvgAProb.cut <- cut(elite$AvgAProb, 124, inclues.lowest=T)
levels(elite$AvgAProb.cut) <- tapply(elite$AvgAProb, elite$AvgAProb.cut, mean)
elite.observed.probabilites.TabA <- prop.table(table(elite$FTR, elite$AvgAProb.cut), 2)[c(1, 2, 3), ]
elite.observed.probabilites.A <- elite.observed.probabilites.TabA[1, ]
elite.bookmaker.probabilites.A <- as.numeric(names(elite.observed.probabilites.A))
#Draws:-
elite$AvgDProb.cut <- cut(elite$AvgDProb, 124, inclues.lowest=T)
levels(elite$AvgDProb.cut) <- tapply(elite$AvgDProb, elite$AvgDProb.cut, mean)
elite.observed.probabilites.TabD <- prop.table(table(elite$FTR, elite$AvgDProb.cut), 2)[c(1, 2, 3), ]
elite.observed.probabilites.D <- elite.observed.probabilites.TabD[2, ]
elite.bookmaker.probabilites.D <- as.numeric(names(elite.observed.probabilites.D))

#Finding R-Squared and RMSE:
elite.bookmaker.probabilities <- c(elite.bookmaker.probabilites.H, elite.bookmaker.probabilites.D, elite.bookmaker.probabilites.A)
elite.observed.probabilities <- c(elite.observed.probabilites.H, elite.observed.probabilites.D, elite.observed.probabilites.A)
#Home Wins
elite.lm.home <- lm(elite.observed.probabilites.H ~ elite.bookmaker.probabilites.H) #Creates the linear model
print(paste("R Squared, Home Win = ", round(summary(elite.lm.home)$r.squared, 5))) #R Squared 
print(paste("RMSE, Home Win = ", round(sqrt(mean(elite.lm.home$residuals^2)), 5))) #RMSE
print(paste("Slope, Home Win = ", round(elite.lm.home$coefficients[2], 5))) #Slope

#Away Wins
elite.lm.away <- lm(elite.observed.probabilites.A ~ elite.bookmaker.probabilites.A)
print(paste("R Squared, Away Win = ", round(summary(elite.lm.away)$r.squared, 5))) #R Squared 
print(paste("RMSE, Away Win = ", round(sqrt(mean(elite.lm.away$residuals^2)), 5))) #RMSE
print(paste("Slope, Away Win = ", round(elite.lm.away$coefficients[2], 5))) #Slope

#Draws
elite.lm.draw <- lm(elite.observed.probabilites.D ~ elite.bookmaker.probabilites.D)
print(paste("R Squared, Draw Win = ", round(summary(elite.lm.draw)$r.squared, 5))) #R Squared 
print(paste("RMSE, Draw Win = ", round(sqrt(mean(elite.lm.draw$residuals^2)), 5))) #RMSE
print(paste("Slope, Draw Win = ", round(elite.lm.draw$coefficients[2], 5))) #Slope

#Overall
elite.linear.model <- lm(elite.observed.probabilities ~ elite.bookmaker.probabilities)
print(paste("R Squared, Overall = ", round(summary(elite.linear.model)$r.squared, 5))) #R Squared 
print(paste("RMSE, Overall = ", round(sqrt(mean(elite.linear.model$residuals^2)), 5))) #RMSE
print(paste("Slope, Overall = ", round(elite.linear.model$coefficients[2], 5))) #Slope

#Final Plot
elite.scatter <- ggplot(data=NULL,aes()) + geom_smooth() +
  geom_jitter(aes(x=elite.bookmaker.probabilites.H, y=elite.observed.probabilites.H, color = "Home Win"), size=0.75, show.legend=T) +
  geom_smooth(aes(x=elite.bookmaker.probabilites.H, y=elite.observed.probabilites.H, color = "Home Win"), method=lm, alpha=.15, size=0.5) +
  geom_jitter(aes(x=elite.bookmaker.probabilites.D, y=elite.observed.probabilites.D, color = "Draw"), size=0.75, show.legend=T) +
  geom_smooth(aes(x=elite.bookmaker.probabilites.D, y=elite.observed.probabilites.D, color = "Draw"), method=lm, alpha=.15, size=0.5) +
  geom_jitter(aes(x=elite.bookmaker.probabilites.A, y=elite.observed.probabilites.A, color = "Away Win"), size=0.75, show.legend = T) +
  geom_smooth(aes(x=elite.bookmaker.probabilites.A, y=elite.observed.probabilites.A, color = "Away Win"), method=lm, alpha=.15, size=0.5) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") + scale_color_manual(name="Bet Type", values=c("Home Win" = "blue", "Draw" = "green4", "Away Win" = "coral")) +
  labs(title="Consensus vs. Observed Probabilities", x="Consensus Probability", y="Observed Probability", caption="Elite Euro. Leagues, 2005-2020\nBin Size: 250 Games") +
  coord_cartesian(xlim=c(0, 1), ylim=c(0, 1)) + theme_light()

ggsave(path = "./writeup/images", filename = "elite_03_scatter.png", plot=elite.scatter, unit="cm", width=15, height=10)

### HAT VALUES AND LEVERAGE PLOT ----
hats <- as.data.frame(hatvalues(elite.lm.draw))
leverageCrit <- (2*3)/nrow(hats)
hats[hats$`hatvalues(elite.lm.draw)` > leverageCrit,]
drawLevPlot <- leveragePlot(elite.lm.draw, elite.bookmaker.probabilites.D, col="green4", id = list(method=list(abs(residuals(elite.lm.draw, type="pearson"))), n=10, cex=0.6, col="red"), xlab = "Consensus Probability (Draw) | Others", ylab = "Observed Probability (Draw) | Others")

### PREDICTIVE PERFORMANCE (Overall P1 and P2 Values) ----
#Calculating Owen (2009)'s P1 and P2 values for overall (all countries)
P1 <- exp( (1/N)*sum(elite$logCorrect) )
P2 <- (1/N)*sum( (1-elite$Correct)**2 + (elite$Incorr1)**2 + (elite$Incorr2)**2 )

### MODELS FOR EACH LEAGUE AND COUNTRY ----
RSqu.H <- NULL; RSqu.D <- NULL; RSqu.A <- NULL; RSqu.O <- NULL
RMSE.H <- NULL; RMSE.D <- NULL; RMSE.A <- NULL; RMSE.O <- NULL
p1.split <- NULL; p2.split <- NULL
slope.H <- NULL; slope.D <- NULL; slope.A <- NULL; slope.O <- NULL

for(i in countries){
  modelTempData <- elite[elite$Country==i, ]
  #Bins
  modelTempData$AvgHProb.cut <- cut(modelTempData$AvgHProb, 20, include.lowest=T)
  modelTempData$AvgDProb.cut <- cut(modelTempData$AvgDProb, 5, include.lowest=T)
  modelTempData$AvgAProb.cut <- cut(modelTempData$AvgAProb, 20, include.lowest=T)
  
  #Means of each bin
  levels(modelTempData$AvgHProb.cut) <- tapply(modelTempData$AvgHProb, modelTempData$AvgHProb.cut, mean)
  levels(modelTempData$AvgDProb.cut) <- tapply(modelTempData$AvgDProb, modelTempData$AvgDProb.cut, mean)
  levels(modelTempData$AvgAProb.cut) <- tapply(modelTempData$AvgAProb, modelTempData$AvgAProb.cut, mean)
  
  #Observed Probability for each bin
  modelTemp.obs.prob.tabH <- prop.table(table(modelTempData$FTR, modelTempData$AvgHProb.cut), 2)[c(1, 2, 3), ]
  modelTemp.obs.prob.tabD <- prop.table(table(modelTempData$FTR, modelTempData$AvgDProb.cut), 2)[c(1, 2, 3), ]
  modelTemp.obs.prob.tabA <- prop.table(table(modelTempData$FTR, modelTempData$AvgAProb.cut), 2)[c(1, 2, 3), ]
  
  modelTemp.obs.prob.H <- modelTemp.obs.prob.tabH[3, ]
  modelTemp.obs.prob.D <- modelTemp.obs.prob.tabD[2, ]
  modelTemp.obs.prob.A <- modelTemp.obs.prob.tabA[1, ]
  
  #Finds the bookmaker probabilities for each group and creates vectors
  modelTemp.boo.prob.H <- as.numeric(names(modelTemp.obs.prob.H))
  modelTemp.boo.prob.D <- as.numeric(names(modelTemp.obs.prob.D))  
  modelTemp.boo.prob.A <- as.numeric(names(modelTemp.obs.prob.A))
  modelTemp.bookmakers <- c(modelTemp.boo.prob.H, modelTemp.boo.prob.D, modelTemp.boo.prob.A)
  modelTemp.observed   <- c(modelTemp.obs.prob.H, modelTemp.obs.prob.D, modelTemp.obs.prob.A)
  
  #Model creation
  modelTempH <- lm(modelTemp.obs.prob.H~modelTemp.boo.prob.H)
  modelTempD <- lm(modelTemp.obs.prob.D~modelTemp.boo.prob.D)  
  modelTempA <- lm(modelTemp.obs.prob.A~modelTemp.boo.prob.A)
  modelTempO <- lm(modelTemp.observed~modelTemp.bookmakers)
  
  #Finding values
  RSqu.H <- c(RSqu.H, round(summary(modelTempH)$r.squared, 5))
  RMSE.H <- c(RMSE.H, round(sqrt(mean(modelTempH$residuals^2)), 5))
  slope.H <- c(slope.H, round(modelTempH$coefficients[2], 5))
  RSqu.D <- c(RSqu.D, round(summary(modelTempD)$r.squared, 5))
  RMSE.D <- c(RMSE.D, round(sqrt(mean(modelTempD$residuals^2)), 5))
  slope.D <- c(slope.D, round(modelTempD$coefficients[2], 5))
  RSqu.A <- c(RSqu.A, round(summary(modelTempA)$r.squared, 5))
  RMSE.A <- c(RMSE.A, round(sqrt(mean(modelTempA$residuals^2)), 5))
  slope.A <- c(slope.A, round(modelTempA$coefficients[2], 5))
  RSqu.O <- c(RSqu.O, round(summary(modelTempO)$r.squared, 5))
  RMSE.O <- c(RMSE.O, round(sqrt(mean(modelTempO$residuals^2)), 5))
  slope.O <- c(slope.O, round(modelTempO$coefficients[2], 5))
  
  p1.temp <- exp((1/(nrow(modelTempData)))*sum(modelTempData$logCorrect))
  p2.temp <- (1/(nrow(modelTempData))) * sum( (1-modelTempData$Correct)**2 + (modelTempData$Incorr1)**2 + (modelTempData$Incorr2)**2 )
  
  p1.split <- c(p1.split, round(p1.temp, 5))
  p2.split <- c(p2.split, round(p2.temp, 5))
}
#Putting this into an easy-to-see Table:
league.values <- matrix(c(RSqu.H, RMSE.H, slope.H, RSqu.D, RMSE.D, slope.D, RSqu.A, RMSE.A, slope.A, RSqu.O, RMSE.O, slope.O, p1.split, p2.split), ncol=6, byrow=T)
rownames(league.values) <- c("RSqu.H", "RMSE.H", "slope.H", "RSqu.D", "RMSE.D", "slope.D", "RSqu.A", "RMSE.A", "slope.A", "RSqu.O", "RMSE.O", "slope.O", "p1.split", "p2.split")
colnames(league.values) <- countries
league.values <- as.table(league.values)

#For each season:
rsqu.season <- NULL; rmse.season <- NULL; p1.season <- NULL; p2.season <- NULL; slope.season <- NULL
for(i in seasons){
  modelTempData <- elite[elite$Season==i, ]
  modelTempData$AvgHProb.cut <- cut(modelTempData$AvgHProb, 10, include.lowest=T)
  modelTempData$AvgDProb.cut <- cut(modelTempData$AvgDProb, 5, include.lowest=T)
  modelTempData$AvgAProb.cut <- cut(modelTempData$AvgAProb, 10, include.lowest=T)
  
  #Finds the mean of each group (cut)
  levels(modelTempData$AvgHProb.cut) <- tapply(modelTempData$AvgHProb, modelTempData$AvgHProb.cut, mean)
  levels(modelTempData$AvgDProb.cut) <- tapply(modelTempData$AvgDProb, modelTempData$AvgDProb.cut, mean)
  levels(modelTempData$AvgAProb.cut) <- tapply(modelTempData$AvgAProb, modelTempData$AvgAProb.cut, mean)
  
  #Finds the observed probability for each cut
  modelTemp.obs.prob.tabH <- prop.table(table(modelTempData$FTR, modelTempData$AvgHProb.cut), 2)[c(1, 2, 3), ]
  modelTemp.obs.prob.tabD <- prop.table(table(modelTempData$FTR, modelTempData$AvgDProb.cut), 2)[c(1, 2, 3), ]
  modelTemp.obs.prob.tabA <- prop.table(table(modelTempData$FTR, modelTempData$AvgAProb.cut), 2)[c(1, 2, 3), ]
  
  modelTemp.obs.prob.H <- modelTemp.obs.prob.tabH[3, ]
  modelTemp.obs.prob.D <- modelTemp.obs.prob.tabD[2, ]
  modelTemp.obs.prob.A <- modelTemp.obs.prob.tabA[1, ]
  
  #Finds the bookmaker probabilities for each group and creates vectors
  modelTemp.boo.prob.H <- as.numeric(names(modelTemp.obs.prob.H))
  modelTemp.boo.prob.D <- as.numeric(names(modelTemp.obs.prob.D))  
  modelTemp.boo.prob.A <- as.numeric(names(modelTemp.obs.prob.A))
  modelTemp.bookmakers <- c(modelTemp.boo.prob.H, modelTemp.boo.prob.D, modelTemp.boo.prob.A)
  modelTemp.observed   <- c(modelTemp.obs.prob.H, modelTemp.obs.prob.D, modelTemp.obs.prob.A)
  
  #Making the model
  modelTempO <- lm(modelTemp.observed~modelTemp.bookmakers)
  #Finding values
  rsqu.season <- c(rsqu.season, round(summary(modelTempO)$r.squared, 5))
  rmse.season <- c(rmse.season, round(sqrt(mean(modelTempO$residuals^2)), 5))
  slope.season <- c(slope.season, round(modelTempO$coefficients[2], 5))
  
  p1.temp <- exp((1/(nrow(modelTempData)))*sum(modelTempData$logCorrect))
  p2.temp <- (1/(nrow(modelTempData))) * sum( (1-modelTempData$Correct)**2 + (modelTempData$Incorr1)**2 + (modelTempData$Incorr2)**2 )
  
  p1.season <- c(p1.season, round(p1.temp, 5))
  p2.season <- c(p2.season, round(p2.temp, 5))
}

season.values <- matrix(c(rsqu.season, rmse.season, p1.season, p2.season, slope.season), ncol=5, byrow=F)
colnames(season.values) <- c("rsqu", "rmse", "p1", "p2", "slope")
rownames(season.values) <- seasons

#Plotting Season Values
rsqu.se.plot <- ggplot(NULL, aes(y=rsqu.season, x=c(2005:2019))) + geom_jitter(color="violetred1")+theme_light() + labs(x = 'Year', y = 'R2') + geom_smooth(method = 'lm', color = 'violetred4', se = F)
rmse.se.plot <- ggplot(NULL, aes(y=rmse.season, x=c(2005:2019))) + geom_jitter(color="steelblue4")+theme_light() + labs(x = 'Year', y = 'RMSE') + geom_smooth(method = 'lm', color = 'steelblue1', se = F)
p1.se.plot <- ggplot(NULL, aes(y=p1.season, x=c(2005:2019))) + geom_jitter(color="darkorange4")+theme_light() + labs(x = 'Year', y = 'P1') + geom_smooth(method = 'lm', color = 'darkorange1', se = F)
p2.se.plot <- ggplot(NULL, aes(y=p2.season, x=c(2005:2019))) + geom_jitter(color="slateblue4")+theme_light() + labs(x = 'Year', y = 'P2') + geom_smooth(method = 'lm', color = 'slateblue1', se = F)
slope.se.plot <- ggplot(NULL, aes(y=slope.season, x=c(2005:2019))) + geom_jitter(color="springgreen3") + theme_light() + labs(x = "Year", y = "Slope") + geom_smooth(method = "lm", color = "springgreen3", se = F) + geom_abline(slope = 0, intercept = 1, color = "black") + coord_cartesian(ylim = c(0.5, 1.5))

seasontimeplot <- grid.arrange(rsqu.se.plot, rmse.se.plot, p1.se.plot, p2.se.plot, slope.se.plot, ggplot(NULL)+geom_blank()+theme_void(), nrow = 3, top = 'Elite Leagues Accuracy Statistics over Time')
ggsave(path="./writeup/images", filename="elite_06_seasontimeplot.png", plot=seasontimeplot, unit="cm", width=20, height=20)

### COMPETITIVE BALANCE PCA ----

##LEAGUE MODEL:
#(For leagues, we have the comp. bal. statistics, unlike for seasons)
#We first define the statistics (Gini, NAMSI and K) from Goossens (05):
namsi <- c(0.374, 0.372, 0.364, 0.342, 0.418, 0.505)
kappa <- c(5.71, 5.79, 5.07, 6.00, 5.36, 4.07); invkap <- 1/kappa
gini  <- c(0.723, 0.826, 0.861, 0.784, 0.737, 0.898)

#We define IMBALANCE (Scale (standardise) each statistic above):
namsisc <- scale(namsi); invkapsc <- scale(invkap); ginisc <- scale(gini)
imbalance <- (namsisc[c(1:6),] + invkapsc[c(1:6),] + ginisc[c(1:6),])/3
#The [c(1:6),] cuts off the sd and mean attributes from the scaled data

#We define the LEVEL OF ATTACK - Shots Per Game / Goals Per Game.
attack <- NULL; attackPO <- NULL
for (l in 1:5){
  for (s in seasons){
    dataTemp <- read.csv(paste0("https://www.football-data.co.uk/mmz4281/", s, "/", co.we[l], ".csv"))
    dataTemp <- dataTemp[ ,c("FTHG", "FTAG", "HS", "AS")]
    dataTemp$totalGoals <- with(dataTemp, FTHG+FTAG)
    dataTemp$totalShots <- with(dataTemp, HS+AS)
    dataTemp <- na.omit(dataTemp)
    attack <- c(attack, mean(dataTemp$totalShots)/mean(dataTemp$totalGoals))
  }
}
for (s in seasons[13:15]){ 
  #Data for Po is only avaliable for the 17/18 season onwards.
  dataTemp <- read.csv(paste0("https://www.football-data.co.uk/mmz4281/", s, "/", co.we[6], ".csv"))
  dataTemp <- dataTemp[ ,c("FTHG", "FTAG", "HS", "AS")]
  dataTemp$totalGoals <- with(dataTemp, FTHG+FTAG)
  dataTemp$totalShots <- with(dataTemp, HS+AS)
  attackPO <- c(attackPO, mean(dataTemp$totalShots)/mean(dataTemp$totalGoals))
}
attack <- matrix(attack, ncol=15, byrow = T)
attackPO <- matrix(attackPO, nrow=1, byrow = T)
colnames(attack) <- seasons; rownames(attack) <- countries[1:5]
colnames(attackPO) <- seasons[13:15]; rownames(attackPO) <- countries[6]

attack.league <- c(mean(attack[1,]), mean(attack[2,]), mean(attack[3,]), mean(attack[4,]), mean(attack[5,]), mean(attackPO[1,]))
attack.season <- NULL
for (i in 1:12){attack.season <- c(attack.season, mean(attack[,i]))}
for (i in 1:3){attack.season <- c(attack.season, mean(c(attack[1,(i+12)], attack[2,(i+12)], attack[3,(i+12)], attack[4,(i+12)], attack[5,(i+12)], attackPO[1,i] )))
}

#We define PredAcc, as the normalised sum of the 4 predictive statistics
#We will take the inverse of RMSE and P2
#A high PredAcc value => better bookmaker performance
invrmse.l <- 1/RMSE.O; invp2.l <- 1/p2.split
rsqu.l.sc <- scale(RSqu.O); invrmse.l.sc <- scale(invrmse.l)
p1.l.sc <- scale(p1.split); invp2.l.sc <- scale(invp2.l)
predacc <- (rsqu.l.sc + invrmse.l.sc + p1.l.sc + invp2.l.sc)/4

pc.league <- matrix(c(imbalance, attack.league, predacc), ncol=3, byrow=F)
colnames(pc.league) <- c("imbalance", "attack", "predacc")
rownames(pc.league) <- countries

league.model <- prcomp(pc.league)
league.model$rotation; summary(league.model)

##SEASON MODEL:
pc.season <- matrix(c(rsqu.season, (1/rmse.season), p1.season, (1/p2.season), attack.season), ncol = 5, byrow=F)
colnames(pc.season) <- c("rsqu", "inv rmse", "p1", "inv p2", "attack")
rownames(pc.season) <- seasons
pc.season.sc <- scale(pc.season)

season.model <- prcomp(pc.season.sc)
summary(season.model); round(season.model$rotation,3)

##PLOTS:
#League Model Plots
leascree <- ggplot(NULL, aes(x = c(1:3), y = (league.model$sdev)^2)) + geom_line() + geom_point(size = 2) + theme_light() + geom_abline(slope = 0, intercept = 1, color = "red") +labs(x = "Principal Component", y = "Variances", title = "Screeplot of League PCA Components")

leamodlabels <- NULL
for(i in countries){
  if(league.model$x[i,1] > 0){
    leamodlabels[i] <- round(league.model$x[i,1],2) + 0.33
  }
  else{leamodlabels[i] <- round(league.model$x[i,1],2) - 0.33}
}

leacomps <- ggplot(NULL, aes(countries, league.model$x[,1], color = countries, label = round(league.model$x[,1],2))) + geom_pointrange(ymin = 0, ymax = league.model$x[,1]) + theme_light() + labs(x = "Country", title = "PC1 Values", y = "Component 1") + theme(legend.position = "none") + scale_x_discrete(labels=c("po" = "Portugal", "it" = "Italy", "fr" = "France", "es" = "Spain", "en" = "England", "de" = "Germany")) + geom_text(aes(y = leamodlabels)) + coord_cartesian(ylim = c(-3,3))


leaguepca <- grid.arrange(leascree, leacomps, ncol = 2, top = "By-League PCA") 
ggsave(path="./writeup/images", filename="elite_07a_leaguepca.png", plot=leaguepca, unit="cm", width=20, height=10)

#Season Model Plots
seascree <- ggplot(NULL, aes(x = c(1:5), y = (season.model$sdev)^2)) + geom_line() + geom_point(size = 2) + theme_light() + geom_abline(slope = 0, intercept = 1, color = "red") +labs(x = "Principal Component", y = "Variances", title = "Screeplot of Season PCA Components")

seacomps <- ggplot(NULL, aes(x = season.model$x[,1], y = season.model$x[,2], label = seasons)) + geom_jitter() + labs(x = 'Component 1', y = 'Component 2', main = "PC1 vs. PC2") + theme_light() + geom_text(aes(x = season.model$x[,1], y = season.model$x[,2]-0.2))

seasonpca <- grid.arrange(seascree, seacomps, ncol = 2, top = "By-Season PCA") 
ggsave(path="./writeup/images", filename="elite_07b_seasonpca.png", plot=seasonpca, unit="cm", width=20, height=10)

#- End -