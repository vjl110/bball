
setwd("C:/Users/Layne/Dropbox/bball")
PLR <- read.csv('data/plr.csv')[-2]
	PLR <- subset(PLR, !duplicated(name))

TOT <- read.csv("data/TOT_pull.csv")[c(1:3, 5:8, 24)]
	TOT$Season <- as.numeric(substr(TOT$Season, 1, 4)) + 1
	TOT <- subset(TOT, !is.na(MP))

	TOT$Tm[TOT$Tm == "NOH"] <- "NOP"
	TOT$Tm[TOT$Tm == "NJN"] <- "BRK"
	TOT$Tm[TOT$Tm == "CHA"] <- "CHO"
TOT <- merge(TOT, PLR, by = "id", all.x = T)
DRFT <- read.csv("data/drafts.csv")
	colnames(DRFT)[2] <- "name"
TOT <- merge(TOT, DRFT, by = "name", all.x = T)

s12 <- subset(TOT, Season == 2012 & Tm != "TOT")
	s12 <- s12[order(s12$MP, decreasing = T), ]
s13 <- subset(TOT, Season == 2013 & Tm != "TOT")
	s13 <- s13[order(s13$MP, decreasing = T), ]
s14 <- subset(TOT, Season == 2014 & Tm != "TOT")
	s14 <- s14[order(s14$MP, decreasing = T), ]
s15 <- subset(TOT, Season == 2015 & Tm != "TOT")
	s15 <- s15[order(s15$MP, decreasing = T), ]


tst <- merge(s12, s13, by = c("id", "Tm"), all.x =T )
tst$Tm <- as.character(tst$Tm)
head(tst)
tmz1 <- c()
for(i in 1:length(unique(tst$Tm))){
	team <- subset(tst, Tm == unique(tst$Tm)[i])
	tmz1[i] <- sum(team$MP.y, na.rm=T)/sum(team$MP.x, na.rm=T)
}
		
tst <- merge(s12, s14, by = c("id", "Tm"), all.x =T )
tst$Tm <- as.character(tst$Tm)
head(tst)
tmz2 <- c()
for(i in 1:length(unique(tst$Tm))){
	team <- subset(tst, Tm == unique(tst$Tm)[i])
	tmz2[i] <- sum(team$MP.y, na.rm=T)/sum(team$MP.x, na.rm=T)
}
		

tst <- merge(s12, s15, by = c("id", "Tm"), all.x =T )
tst$Tm <- as.character(tst$Tm)
head(tst)
tmz3 <- c()
for(i in 1:length(unique(tst$Tm))){
	team <- subset(tst, Tm == unique(tst$Tm)[i])
	tmz3[i] <- sum(team$MP.y, na.rm=T)/sum(team$MP.x, na.rm=T)
}


ATL
BRK
TOR
WSH
CHI
MIL
CLE
BOS
GSW
NOP
POR
MEM
LAC
SAS
HOU
DAL

