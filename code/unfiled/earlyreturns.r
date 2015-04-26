rm(list=ls())
setwd("~/GitHub/bball/data")

load("nba")
nba$PRB <- 0
nba$PRB[nba$OBS >= 2.5] <- 1
nba$PRB[nba$OBS >= 5] <- 2
nba$PRB[nba$OBS >= 7.5] <- 3
nba$PRB[nba$OBS >= 10] <- 4

OBS <- nba 

###
###### ARRANGE NBA DATA
###

### prep WS set
nba <- read.csv("BBRadv.csv", strip.white = T)
nba <- subset(nba, Tm != "TOT")
nba$Pos[is.na(nba$Pos)] <- 0
nba.sum <- nba[-c(3:7, 9:23, 25)]
nba.mean <- nba[-c(4:5, 7:25)]
nba.sum <- aggregate( . ~ Name + Season, data = nba.sum, sum)
nba.mean <- aggregate( . ~ Name + Season, data = nba.mean, mean)
nba <- merge(nba.mean, nba.sum, by=c('Name','Season'))
nba$WS[nba$Season==1999] <- nba$WS[nba$Season==1999]*(82/50)
nba$WS[nba$Season==2012] <- nba$WS[nba$Season==2012]*(82/66)

### add rapm data
rapm <- read.csv("rapm.csv", strip.white = T)
rapm <- rapm[-6]
nba <- merge(nba, rapm, by=c("Name", "Season"), all.x=T)
nba$RAPM <- round((nba$DIF + 3.5)*(nba$MP/3936)*3.15, digits= 2)
nba$RAPM[nba$Season==1999] <- nba$RAPM[nba$Season==1999]*(82/50)
nba$RAPM[nba$Season==2012] <- nba$RAPM[nba$Season==2012]*(82/66)
	# scale obs
mn.WS <- mean(nba$WS, na.rm=T)
sd.WS <- sd(nba$WS, na.rm=T)
nba$RAPM <- mn.WS + scale(nba$RAPM)*sd.WS
nba$CMB <- as.vector(ifelse(is.na(nba$RAPM), nba$WS, (nba$WS + nba$RAPM)/2))


data <- merge(nba, OBS, by = "Name", all.x = T)

bio <- read.csv("BBRbio.csv", strip.white = T)
data <- merge(data, bio, by="Name", all.x=T)
data$Age <- ((data$Season-1900)*365 - data$DOB)/365


retro <- read.csv("RETRO.csv")
retro <- retro[c(1:2, 5, 13)]
data <- merge(data, retro, by="Name", all.x=T)

ac <- read.csv("agecurve.csv")
ac.lws <- loess(ac$Curve.cen ~ ac$Age, span = 1)

lws.pick <- loess(data$OBS ~ data$Pick, span=0.85)
data <- subset(data, Pick <= 30)

data$Pick.adj <- predict(lws.pick, newdata=data$Pick)
data$Pick.adj[is.na(data$Pick.adj)] <- min(data$Pick.adj, na.rm=T)
data <- data[-c(6:10, 12, 17:18)]

data$EXP <- data$Season - data$DRFT
data$EXP[data$Name == "Jonas Valanciunas"] <- data$EXP[data$Name == "Jonas Valanciunas"] - 1
data$EXP[data$Name == "Ricky Rubio"] <- data$EXP[data$Name == "Ricky Rubio"] - 2
data <- subset(data, EXP <= 3 & EXP > 0)
data <- na.omit(data)

for(i in 1:nrow(data)){
	data$PST[i] <- ifelse(length(data$CMB[data$Name == data$Name[i] & data$Season < data$Season[i]]) > 0,
	weighted.mean(data$CMB[data$Name == data$Name[i] & data$Season < data$Season[i]], data$MP[data$Name == data$Name[i] & data$Season < data$Season[i]], na.rm= T) 
	, NA)
}


library(nnet)
	rook <- multinom(PRB ~ scale(I(predict(ac.lws, newdata= Age))) + scale(CMB) + scale(Pick.adj) + scale(EWP) + scale(Height)
                    , data=subset(data, EXP == 1), weights = MP)
summary(rook)
	soph <- multinom(PRB ~ scale(I(predict(ac.lws, newdata= Age))) + scale(CMB) + scale(PST) + scale(Pick.adj) + scale(EWP) + scale(Height)
                    , data=subset(data, EXP == 2), weights = MP)
summary(soph)
	juni <- multinom(PRB ~ scale(I(predict(ac.lws, newdata= Age))) + scale(CMB) + scale(PST) + scale(EWP) + scale(Height)
                    , data=subset(data, EXP == 3), weights = MP)
summary(juni)

data$BUST = NA
data$BNCH = NA
data$STRT = NA
data$STUD = NA
data$STAR = NA

data[data$EXP == 1, ncol(data) - c(4:0)] <- round(predict(rook, newdata=subset(data, EXP ==1), "probs"), digits=3)
data[data$EXP == 2, ncol(data) - c(4:0)] <- round(predict(soph, newdata=subset(data, EXP ==2), "probs"), digits=3)
data[data$EXP == 3, ncol(data) - c(4:0)] <- round(predict(juni, newdata=subset(data, EXP ==3), "probs"), digits=3)


PRB <- data.frame("Name" = data$Name, "EXP" = data$EXP, "Season" = data$Season, "EWP" = round(data$EWP, 1), "PST" = round(data$PST, 1), "CRNT" = round(data$CMB, 1), "BUST" = data$BUST, "BNCH" = data$BNCH, "STRT"=data$STRT, "STUD" = data$STUD, "STAR"=data$STAR, "Pick" = data$Pick)
PRB$BNCH[PRB$CRNT >= 2.5] <- PRB$BNCH[PRB$CRNT >= 2.5]  + PRB$BUST[PRB$CRNT >= 2.5] 
PRB$BUST[PRB$CRNT >= 2.5] <- 0
PRB$STRT[PRB$CRNT >= 5] <- PRB$STRT[PRB$CRNT >= 5] + PRB$BNCH[PRB$CRNT >= 5]
PRB$BNCH[PRB$CRNT >= 5] <- 0
PRB$STRT[PRB$CRNT >= 7.5] <- PRB$STUD[PRB$CRNT >= 7.5] + PRB$STRT[PRB$CRNT >= 7.5]
PRB$BNCH[PRB$CRNT >= 7.5] <- 0
PRB$STUD[PRB$CRNT >= 10] <- PRB$STAR[PRB$CRNT >= 10] + PRB$STUD[PRB$CRNT >= 10]
PRB$STRT[PRB$CRNT >= 10] <- 0





