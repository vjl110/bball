setwd("~/GitHub/bball/scraper")


library(XML)
library(RCurl)
library(httr)


###
######  LINEUP DATA ####
###
lnp.lst <- list()
for(y in 2001:2015){

	theurl <- paste(c("http://www.basketball-reference.com/play-index/plus/lineup_finder.cgi?request=1&match=single&player_id=&lineup_type=5-man&output=total&year_id=", y, "&is_playoffs=&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&c1stat=fg&c1comp=ge&c1val=&c2stat=opp_fg&c2comp=ge&c2val=&c3stat=ast&c3comp=ge&c3val=&c4stat=opp_ast&c4comp=ge&c4val=&order_by=orb&order_by_asc=&offset=0"),sep="",collapse="")
	tab <- GET(theurl)
		tab <- readHTMLTable(rawToChar(tab$content), stringsAsFactors = F)
			tab <- as.data.frame(tab)
			tab <- tab[-c(1:5)]
	base <- subset(tab, stats.MP != "MP" & !is.na(stats.MP))
	i <- 0
	kill <- 0
	while(kill == 0){
		i <- i + 100
		theurl <- paste(c("http://www.basketball-reference.com/play-index/plus/lineup_finder.cgi?request=1&player_id=&match=single&lineup_type=5-man&output=total&year_id=", y, "&is_playoffs=&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&c1stat=fg&c1comp=ge&c1val=&c2stat=opp_fg&c2comp=ge&c2val=&c3stat=ast&c3comp=ge&c3val=&c4stat=opp_ast&c4comp=ge&c4val=&order_by=orb&order_by_asc=&offset=", 0 + i),sep="",collapse="")
		tab <- GET(theurl)
			tab <- readHTMLTable(rawToChar(tab$content), stringsAsFactors = F)
				tab <- as.data.frame(tab)
				tab <- tab[-c(1:5)]		
		if(length(tab$stats.MP) == 0){
			kill <- 1
		}else{
			tab <- subset(tab, stats.MP != "MP" & !is.na(stats.MP)) 
			base <- rbind(base, tab)
		}
	}		
lnp <- base
write.csv(lnp, paste(c("lineup", y, ".csv"),sep="",collapse=""))
#	lnp.lst[[y - 2000]] <- lnp
}




#############
#############
##########


lnp <- read.csv("lineup2001.csv")
for(i in 2002:2014){
	hld <- read.csv(paste(c("lineup", i, ".csv"),sep="",collapse=""))
	lnp <- rbind(lnp, hld)
}

####
#######
###


indi <- read.csv("indi_data.csv")
lnp <- read.csv("lineup_data.csv")


for(i in c(9:26, 29:42)){
	indi[,i] <- (indi[,i]/indi$Poss.O_indi)*100
}

indi$RIM <- indi$LAY.UP + indi$DUNK + indi$TIP_SHOT 
indi$RIM.a <- indi$LAY.UP.a + indi$DUNK.a + indi$TIP_SHOT.a 
indi$RIM.asd <- indi$LAY.UP.asd + indi$DUNK.asd

indi$FTR <- indi$FT/indi$FGA




lnp <- subset(lnp, Season > 2001)
lnp$Name1 <- as.character(lnp$Name1)
lnp$Name2 <- as.character(lnp$Name2)
lnp$Name3 <- as.character(lnp$Name3)
lnp$Name4 <- as.character(lnp$Name4)
lnp$Name5 <- as.character(lnp$Name5)
indi$Name <- as.character(indi$Name)

lnp <- subset(lnp, Poss.O_lnp >= 20)

for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$ORB.[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$ORB.[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$ORB.[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$ORB.[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$ORB.[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$ORB1[i] <- vals[1]
		lnp$ORB2[i] <- vals[2]
		lnp$ORB3[i] <- vals[3]
		lnp$ORB4[i] <- vals[4]
		lnp$ORB5[i] <- vals[5]
	}else{
	}
}
for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$DRB.[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$DRB.[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$DRB.[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$DRB.[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$DRB.[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$DRB1[i] <- vals[1]
		lnp$DRB2[i] <- vals[2]
		lnp$DRB3[i] <- vals[3]
		lnp$DRB4[i] <- vals[4]
		lnp$DRB5[i] <- vals[5]
	}else{
	}
}
for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$TOV.[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$TOV.[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$TOV.[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$TOV.[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$TOV.[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$TOV1[i] <- vals[1]
		lnp$TOV2[i] <- vals[2]
		lnp$TOV3[i] <- vals[3]
		lnp$TOV4[i] <- vals[4]
		lnp$TOV5[i] <- vals[5]
	}else{
	}
}
for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$AST.[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$AST.[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$AST.[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$AST.[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$AST.[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$AST1[i] <- vals[1]
		lnp$AST2[i] <- vals[2]
		lnp$AST3[i] <- vals[3]
		lnp$AST4[i] <- vals[4]
		lnp$AST5[i] <- vals[5]
	}else{
	}
}
for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$eFG.[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$eFG.[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$eFG.[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$eFG.[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$eFG.[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$eFG1[i] <- vals[1]
		lnp$eFG2[i] <- vals[2]
		lnp$eFG3[i] <- vals[3]
		lnp$eFG4[i] <- vals[4]
		lnp$eFG5[i] <- vals[5]
	}else{
	}
}
for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$FTR[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$FTR[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$FTR[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$FTR[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$FTR[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$FTR1[i] <- vals[1]
		lnp$FTR2[i] <- vals[2]
		lnp$FTR3[i] <- vals[3]
		lnp$FTR4[i] <- vals[4]
		lnp$FTR5[i] <- vals[5]
	}else{
	}
}



lnp$ORB.sum <- lnp$ORB1 + lnp$ORB2 + lnp$ORB3 + lnp$ORB4 + lnp$ORB5
lnp$DRB.sum <- lnp$DRB1 + lnp$DRB2 + lnp$DRB3 + lnp$DRB4 + lnp$DRB5
lnp$TOV.sum <- lnp$TOV1 + lnp$TOV2 + lnp$TOV3 + lnp$TOV4 + lnp$TOV5
lnp$AST.sum <- lnp$AST1 + lnp$AST2 + lnp$AST3 + lnp$AST4 + lnp$AST5
lnp$eFG.sum <- lnp$eFG1 + lnp$eFG2 + lnp$eFG3 + lnp$eFG4 + lnp$eFG5
lnp$FTR.sum <- lnp$FTR1 + lnp$FTR2 + lnp$FTR3 + lnp$FTR4 + lnp$FTR5


m1 <- lm(FFO.reb ~ ORB1 + ORB2 + ORB3 + ORB4 + ORB5, data = lnp, weight = Poss.O_lnp)
m2 <- lm(FFD.reb ~ DRB1 + DRB2 + DRB3 + DRB4 + DRB5, data = lnp, weight = Poss.D_lnp)
m3 <- lm(FFO.tov ~ TOV1 + TOV2 + TOV3 + TOV4 + TOV5 + AST.sum, data = lnp, weight = Poss.O_lnp)
m4 <- lm(FFO.efg ~ AST1 + AST2 + AST3 + AST4 + AST5, data = lnp, weight = Poss.O_lnp)
m5 <- lm(FFO.efg ~ eFG1 + eFG2 + eFG3 + eFG4 + eFG5 + AST1 + AST2 + AST3 + AST4 + AST5, data = lnp, weight = Poss.O_lnp)
m6 <- lm(FFO.ftr ~ FTR1 + FTR2 + FTR3 + FTR4 + FTR5, data = lnp, weight = Poss.O_lnp)






png(filename="drbplot.png", height = 500, width = 500)
plot(NULL, xlim = c(.5, 1), ylim = c(.5, 1), ylab = "Observed DRB%", xlab = "Summed individual DRB%")
points(lnp$DRB.lnp[lnp$Poss.D_lnp > 100], lnp$FFD.reb[lnp$Poss.D_lnp > 100])
fit <- loess(lnp$FFD.reb[lnp$Poss.D_lnp > 100] ~ lnp$DRB.lnp[lnp$Poss.D_lnp > 100])
lines(c(1:100)/100, predict(fit, c(1:100)/100), col = "red")
dev.off()

png(filename="orbplot.png", height = 500, width = 500)
plot(NULL, xlim = c(.1, .5), ylim = c(.05, .6), ylab = "Observed ORB%", xlab = "Summed individual ORB%")
points(lnp$ORB.lnp[lnp$Poss.O_lnp > 100], lnp$FFO.reb[lnp$Poss.O_lnp > 100])
fit <- loess(lnp$FFO.reb[lnp$Poss.O_lnp > 100] ~ lnp$ORB.lnp[lnp$Poss.O_lnp > 100])
lines(c(1:100)/100, predict(fit, c(1:100)/100), col = "red")
dev.off()

png(filename="tovplot.png", height = 500, width = 500)
plot(NULL, xlim = c(0.5, 1), ylim = c(0, 0.35), ylab = "Observed TOV%", xlab = "Summed individual TOV%")
points(lnp$TOV.lnp[lnp$Poss.O_lnp > 100], lnp$FFO.tov[lnp$Poss.O_lnp > 100])
fit <- loess(lnp$FFO.tov[lnp$Poss.O_lnp > 100] ~ lnp$TOV.lnp[lnp$Poss.O_lnp > 100])
lines(c(1:100)/100, predict(fit, c(1:100)/100), col = "red")
dev.off()


png(filename="efgplot.png", height = 500, width = 500)
plot(NULL, xlim = c(.3, .7), ylim = c(.3, .7), ylab = "Observed eFG%", xlab = "Summed individual eFG%")
points(lnp$EFG.lnp[lnp$Poss.O_lnp > 100]/5, lnp$FFO.efg[lnp$Poss.O_lnp > 100])
dev.off()

png(filename="efgplot.png", height = 500, width = 500)
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), ylab = "Observed eFG%", xlab = "Summed individual usage%")
points(lnp$USG.lnp[lnp$Poss.O_lnp > 100]/5, lnp$FFO.efg[lnp$Poss.O_lnp > 100])
dev.off()














lnp <- read.csv("lineup_data.csv")
lnp$P1 <- as.character(lnp$P1)
lnp$P2 <- as.character(lnp$P2)
lnp$P3 <- as.character(lnp$P3)
lnp$P4 <- as.character(lnp$P4)
lnp$P5 <- as.character(lnp$P5)
nmr <- read.csv("lineup_namer.csv")
nmr$Name2 <- as.character(nmr$Name2)

colnames(nmr)[1] <- "Name1"
colnames(nmr)[2] <- "P1"
lnp <- merge(lnp, nmr, by = c("P1", "Season", "Tm"), all.x = T)
colnames(nmr)[1] <- "Name2"
colnames(nmr)[2] <- "P2"
lnp <- merge(lnp, nmr, by = c("P2", "Season", "Tm"), all.x = T)
colnames(nmr)[1] <- "Name3"
colnames(nmr)[2] <- "P3"
lnp <- merge(lnp, nmr, by = c("P3", "Season", "Tm"), all.x = T)
colnames(nmr)[1] <- "Name4"
colnames(nmr)[2] <- "P4"
lnp <- merge(lnp, nmr, by = c("P4", "Season", "Tm"), all.x = T)
colnames(nmr)[1] <- "Name5"
colnames(nmr)[2] <- "P5"
lnp <- merge(lnp, nmr, by = c("P5", "Season", "Tm"), all.x = T)

write.csv(lnp, "fullname_lineup.csv")




