rm(list=ls())
#setwd("C:/Users/Layne/Dropbox/Archive/BBall/Draft proj")
setwd("C:/Users/SCAN Project/Dropbox/Archive/BBall/Draft proj")

#######

nba <- read.csv("BBRtot.csv")
nba <- subset(nba, Tm != "TOT")
nba.sum <- nba[-c(3:6, 8)]
nba.mean <- nba[-c(4:5, 7:24)]
nba.sum <- aggregate( . ~ Name + Season, data = nba.sum, sum)
nba.mean <- aggregate( . ~ Name + Season, data = nba.mean, mean)
nba <- merge(nba.mean, nba.sum, by=c('Name','Season'))

nba <- subset(nba, Age >= 24 & Age <= 27)
nba <- aggregate( . ~ Name, data = nba, mean)

#nba <- subset(nba, MP > 500)

for(i in 7:ncol(nba)){
	nba[,i] <- (nba[,i]/nba[,6])*36
}

nba$X2PER <- nba$X2P / nba$X2PA
nba$X3PER <- nba$X3P / nba$X3PA
nba$FTPER <- nba$FT / nba$FTA
nba$MPG <- nba$MP / nba$G

nba <- nba[-c(2:5, 7, 9, 11, 13:14)]

colnames(nba) <- c("Name", "MP.nba", "X2PA.nba", "X3PA.nba", "FTA.nba", "TRB.nba", "AST.nba", "STL.nba", "BLK.nba", "TOV.nba", "PF.nba", "PTS.nba", "X2PER.nba", "X3PER.nba", "FTPER.nba", "MPG.nba")

##

load("data")
ncaa <- data[-c(2, 21, 23, 25, 28:39)]
#ncaa$ATO <- ncaa$AST/ncaa$TOV
#ncaa$X2PER <- ncaa$X2P/ncaa$X2PA
#ncaa$X3PER <- ncaa$X3P/ncaa$X3PA
#ncaa$X3PER[is.na(ncaa$X3PER)] <- 0
#ncaa$FTPER <- ncaa$FT/ncaa$FTA
ncaa <- subset(ncaa, !is.na(STL) & !is.na(AST) & !is.na(TOV) & !is.na(PF) & !is.na(BLK)) 

##

dat <- merge(ncaa, nba, by="Name", all.x = T)
#dat <- dat[-2]
dat <- subset(dat, !is.na(STL.nba)) 
#dat$X3PER.y[is.na(dat$X3PER.y)] <- 0

library(MASS)
# X2PA
hld <- lm(X2PA.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = subset(dat, MP.nba > 300), weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#mod2PA <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))	#BIC
	mod2PA <- stepAIC(hld, trace = FALSE)	#AIC
# X3PA
hld <- lm(X3PA.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = subset(dat, MP.nba > 300), weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#mod3PA <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))
	mod3PA <- stepAIC(hld, trace = FALSE)
# FTA
hld <- lm(FTA.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = subset(dat, MP.nba > 300), weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#modFTA <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))
	modFTA <- stepAIC(hld, trace = FALSE)
# TRB
hld <- lm(TRB.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = subset(dat, MP.nba > 300), weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#modTRB <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))
	modTRB <- stepAIC(hld, trace = FALSE)
# AST
hld <- lm(AST.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = subset(dat, MP.nba > 300), weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#modAST <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))
	modAST <- stepAIC(hld, trace = FALSE)
# STL
hld <- lm(STL.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = subset(dat, MP.nba > 300), weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#modSTL <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))
	modSTL <- stepAIC(hld, trace = FALSE)
# BLK
hld <- lm(BLK.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = subset(dat, MP.nba > 300), weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#modBLK <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))
	modBLK <- stepAIC(hld, trace = FALSE)
# TOV
hld <- lm(TOV.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = subset(dat, MP.nba > 300), weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#modTOV <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))
	modTOV <- stepAIC(hld, trace = FALSE)
# PF
hld <- lm(PF.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = subset(dat, MP.nba > 300), weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#modPF <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))
	modPF <- stepAIC(hld, trace = FALSE)
# 2PER
hld <- lm(X2PER.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = subset(dat, MP.nba > 300), weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#mod2PER <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))
	mod2PER <- stepAIC(hld, trace = FALSE)
# 3PER
hld <- lm(X3PER.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = subset(dat, MP.nba > 300 & X3PA.nba > 0.25), weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#mod3PER <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))
	mod3PER <- stepAIC(hld, trace = FALSE)
# FTPER
hld <- lm(FTPER.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = subset(dat, MP.nba > 300), weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#modFTPER <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))
	modFTPER <- stepAIC(hld, trace = FALSE)
# MPG
hld <- lm(MPG.nba ~ Age + SOS + MOV + MP + X2P*X2PA + X3P*X3PA + FT*FTA + TRB + AST*TOV + STL + BLK + PF + PTS + Pos*Height*Weight +
	  log(Age) + log(MP) + log1p(STL) + log1p(BLK) + log1p(TRB) + log1p(AST) + log1p(PF) +
	  I(X2PA/(X3PA + X2PA)) + I(FTA/(X3PA + X2PA))
	  , data = dat, weights = (scale(MP) + abs(min(scale(MP)))) + (scale(Season) + abs(min(scale(Season)))))
	#modMPG <- stepAIC(hld, trace = FALSE, k = log(nrow(step)))
	modMPG <- stepAIC(hld, trace = FALSE)

ps <- subset(ncaa, MP > 300)



MPG.hld <- predict(modMPG, newdat = ps)
MPG.hld <- rank(MPG.hld, na.last = "keep")/max(rank(MPG.hld, na.last = "keep"), na.rm=T)
ps$MPG.p <- round(quantile(na.omit(dat$MPG.nba), MPG.hld), digits = 1)
#
X2PA.hld <- predict(mod2PA, newdat = ps)
X2PA.hld <- rank(X2PA.hld, na.last = "keep")/max(rank(X2PA.hld, na.last = "keep"), na.rm=T)
ps$X2PA.p <- round(quantile(na.omit(subset(dat, MP.nba > 300)$X2PA.nba), X2PA.hld), digits = 1)
#
X3PA.hld <- predict(mod3PA, newdat = ps)
X3PA.hld <- rank(X3PA.hld, na.last = "keep")/max(rank(X3PA.hld, na.last = "keep"), na.rm=T)
ps$X3PA.p <- round(quantile(na.omit(subset(dat, MP.nba > 300)$X3PA.nba), X3PA.hld), digits = 1)
#
FTA.hld <- predict(modFTA, newdat = ps)
FTA.hld <- rank(FTA.hld, na.last = "keep")/max(rank(FTA.hld, na.last = "keep"), na.rm=T)
ps$FTA.p <- round(quantile(na.omit(subset(dat, MP.nba > 300)$FTA.nba), FTA.hld), digits = 1)
#
TRB.hld <- predict(modTRB, newdat = ps)
TRB.hld <- rank(TRB.hld, na.last = "keep")/max(rank(TRB.hld, na.last = "keep"), na.rm=T)
ps$TRB.p <- round(quantile(na.omit(subset(dat, MP.nba > 300)$TRB.nba), TRB.hld), digits = 1)
#
AST.hld <- predict(modAST, newdat = ps)
AST.hld <- rank(AST.hld, na.last = "keep")/max(rank(AST.hld, na.last = "keep"), na.rm=T)
ps$AST.p <- round(quantile(na.omit(subset(dat, MP.nba > 300)$AST.nba), AST.hld), digits = 1)
#
STL.hld <- predict(modSTL, newdat = ps)
STL.hld <- rank(STL.hld, na.last = "keep")/max(rank(STL.hld, na.last = "keep"), na.rm=T)
ps$STL.p <- round(quantile(na.omit(subset(dat, MP.nba > 300)$STL.nba), STL.hld), digits = 1)
#
BLK.hld <- predict(modBLK, newdat = ps)
BLK.hld <- rank(BLK.hld, na.last = "keep")/max(rank(BLK.hld, na.last = "keep"), na.rm=T)
ps$BLK.p <- round(quantile(na.omit(subset(dat, MP.nba > 300)$BLK.nba), BLK.hld), digits = 1)
#
TOV.hld <- predict(modTOV, newdat = ps)
TOV.hld <- rank(TOV.hld, na.last = "keep")/max(rank(TOV.hld, na.last = "keep"), na.rm=T)
ps$TOV.p <- round(quantile(na.omit(subset(dat, MP.nba > 300)$TOV.nba), TOV.hld), digits = 1)
#
PF.hld <- predict(modPF, newdat = ps)
PF.hld <- rank(PF.hld, na.last = "keep")/max(rank(PF.hld, na.last = "keep"), na.rm=T)
ps$PF.p <- round(quantile(na.omit(subset(dat, MP.nba > 300)$PF.nba), PF.hld), digits = 1)
#
X2PER.hld <- predict(mod2PER, newdat = ps)
X2PER.hld <- rank(X2PER.hld, na.last = "keep")/max(rank(X2PER.hld, na.last = "keep"), na.rm=T)
ps$X2PER.p <- round(quantile(na.omit(subset(dat, MP.nba > 300)$X2PER.nba), X2PER.hld), digits = 2)
#
X3PER.hld <- predict(mod3PER, newdat = ps)
X3PER.hld <- rank(X3PER.hld, na.last = "keep")/max(rank(X3PER.hld, na.last = "keep"), na.rm=T)
ps$X3PER.p <- round(quantile(na.omit(subset(dat, MP.nba > 750 & X3PA.nba > 0.5)$X3PER.nba), X3PER.hld), digits = 2)
#
FTPER.hld <- predict(modFTPER, newdat = ps)
FTPER.hld <- rank(FTPER.hld, na.last = "keep")/max(rank(FTPER.hld, na.last = "keep"), na.rm=T)
ps$FTPER.p <- round(quantile(na.omit(subset(dat, MP.nba > 500 &FTA.nba > 1)$FTPER), FTPER.hld), digits = 2)

ps$PTS.p <- round(ps$X2PA.p*ps$X2PER.p*2 + ps$X3PA.p*ps$X3PER.p*3 + ps$FTA.p*ps$FTPER.p, digits = 1)

tst <- ps[order(ps$Season, decreasing = T),]
tst <- subset(tst, !duplicated(Name))






#tst <- subset(tst, Season == 2014)
tst <- tst[-c(3:23)]
#tst$X3PER.p[tst$X3PA.p == 0] <- NA

tst <- subset(tst, Name != 'Frank Kaminsky' & Name != 'Sam Dekker' & Name != 'Delon Wright' & Name != 'Brice Johnson' & Name != 'R.J. Hunter' & Name != 'Bobby Portis' & Name != 'Fred VanFleet' & Name != 'Larry Nance \'14' & Name != 'Montrezl Harrell' & Name != 'Jerian Grant' & Name != 'Jordan Mickey' & Name != 'Chris Obekpa' & Name != 'Briante Weber' & Name != 'Rondae Hollis-Jefferson' & Name != 'Willie Cauley' & Name != 'Alex Hamilton' & Name != 'Shannon Scott' & Name != 'Alan Williams' & Name != 'Chris Horton' & Name != 'Keifer Sykes' & Name != 'Branden Dawson' & Name != 'Aaron White' & Name != 'Nigel Williams-Goss' & Name != 'Perry Ellis' & Name != 'Marcus Paige' & Name != 'Josh Scott' & Name != 'Ron Baker' & Name != 'Aaron Harrison' & Name != 'Andrew Harrison' & Name != 'Josh Hart' & Name != 'Michael Qualls' & Name != 'Kasey Hill' & Name != 'Eric Mika' & Name != 'Brandon Ashley' & Name != 'Mike Tobey' & Name != 'Treveon Graham' & Name != 'Kevin Pangos' & Name != 'Trevor Cooney' & Name != 'Rasheed Sulaimon' & Name != 'Przemek Karnowski' & Name != 'Brad Waldow' & Name != 'Kris Dunn' & Name != 'Cameron Ridley' & Name != 'Yogi Ferrell' & Name != 'A.J. Hammons' & Name != 'Sindarius Thornwell' & Name != 'Troy Williams' & Name != 'Jabari Bird' & Name != 'Wayne Blackshear' & Name != 'Roscoe Smith' & Name != 'Tyler Haws' & Name != 'Gabriel Olaseni' & Name != 'Kaleb Tarczewski' & Name != 'Alex Kirk' & Name != 'Wayne Selden' & Name != 'Terran Petteway' & Name != 'Austin Nichols' & Name != 'Joseph Young' & Name != 'Winston Shepard' & Name != 'Alex Poythress' & Name != 'Jakarr Sampson' & Name != 'Anthony Barber' & Name != 'Jarell Martin' & Name != 'Mamadou Ndiaye' & Name != 'Shaq Goodwin' & Name != 'Shawn Long' & Name != 'Markus Kennedy' & Name != 'Derrick Marks' & Name != 'Andre Hollins' & Name != 'Isaiah Austin')


write.csv(tst, "boxscore_preds.csv")


BSG <- data.frame("Name" = tst$Name, "Season" = tst$Season,
		  "MP" = tst$MPG.p,
		  "PTS" = round((tst$PTS.p / 36) * tst$MPG.p, digits = 1),
#		  "FGA" = round(((tst$X2PA.p + tst$X3PA.p) / 36) * tst$MPG.p, digits = 1),
		  "EFG" = round((tst$X2PA.p*tst$X2PER.p + tst$X3PA.p*tst$X3PER.p*1.5) / (tst$X2PA.p + tst$X3PA.p), digits = 2),
		  "AST" = round((tst$AST.p / 36) * tst$MPG.p, digits = 1),
		  "TOV" = round((tst$TOV.p / 36) * tst$MPG.p, digits = 1),
		  "REB" = round((tst$TRB.p / 36) * tst$MPG.p, digits = 1),
		  "STL" = round((tst$STL.p / 36) * tst$MPG.p, digits = 1),
		  "BLK" = round((tst$BLK.p / 36) * tst$MPG.p, digits = 1),
		  "PF" = round((tst$PF.p / 36) * tst$MPG.p, digits = 1))
