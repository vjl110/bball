setwd("~/GitHub/bball")

indi <- read.csv("data/indi2lineup_data.csv", strip.white = T)

lnp <- read.csv("data/lineup_data.csv", strip.white = T)
	lnp <- subset(lnp, Poss.O_lnp > 50)

lnp <- subset(lnp, Season > 2005)
lnp$Name1 <- as.character(lnp$Name1)
lnp$Name2 <- as.character(lnp$Name2)
lnp$Name3 <- as.character(lnp$Name3)
lnp$Name4 <- as.character(lnp$Name4)
lnp$Name5 <- as.character(lnp$Name5)
indi$Name <- as.character(indi$Name)

indi$Tm <- as.character(indi$Tm)
lnp$Tm <- as.character(lnp$Tm)

# Organize Indi dataset

	# ADDDDD TIPS ONCE I GET INTERNONEONTZZZZ

indi$MID <- indi$JMP + indi$HOOK 
indi$MIDA <- indi$JMPA + indi$HOOKA  

indi$RIM <- indi$LAY + indi$DNK + indi$TIP
indi$RIMA <- indi$LAYA + indi$DNKA + indi$TIPA

indi$FG <- indi$X2P + indi$X3P
indi$FGA <- indi$X2PA + indi$X3PA

indi$FTR <- indi$FT/(indi$X2PA + indi$X3PA)
	
	# convert basic stats to per100
for(i in c(10:23, 39:48, 50:55)){
	indi[,i] <- (indi[,i]/indi$Poss_O)*100
}

	getB <- read.csv("data/getbucks_off.csv", strip.white = T)
	getB <- getB[-c(8:10)]
	indi <- merge(indi, getB, by = c("Name", "Season"), all.x = T) 



# PUlLING DATA FOR LINEUPS

for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$O.eFG[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.eFG[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.eFG[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.eFG[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.eFG[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$O.eFG[i] <- vals[1] + vals[2] + vals[3] + vals[4] + vals[5]
	}else{
	}
}

for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$O.TOV[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.TOV[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.TOV[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.TOV[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.TOV[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$O.TOV[i] <- vals[1] + vals[2] + vals[3] + vals[4] + vals[5]
	}else{
	}
}

for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$O.REB[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.REB[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.REB[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.REB[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.REB[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$O.REB[i] <- vals[1] + vals[2] + vals[3] + vals[4] + vals[5]
	}else{
	}
}

for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$O.FT[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.FT[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.FT[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.FT[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$O.FT[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$O.FT[i] <- vals[1] + vals[2] + vals[3] + vals[4] + vals[5]
	}else{
	}
}









for(i in 1:nrow(lnp)){   # 
	att <- c(indi$FGA[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$FGA[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$FGA[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$FGA[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$FGA[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	efg <- c(indi$eFG.[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$eFG.[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$eFG.[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$eFG.[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$eFG.[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])	
	if(length(att) == 5 & length(efg) == 5){
		vals <- data.frame(att, efg)
		vals <- vals[order(vals$att, decreasing = T), ]		
			lnp$ATT1[i] <- vals[1,1]
				lnp$eFG1[i] <- vals[1,2]	
			lnp$ATT2[i] <- vals[2,1]
				lnp$eFG2[i] <- vals[2,2]
			lnp$ATT3[i] <- vals[3,1]
				lnp$eFG3[i] <- vals[3,2]
			lnp$ATT4[i] <- vals[4,1]
				lnp$eFG4[i] <- vals[4,2]
			lnp$ATT5[i] <- vals[5,1]
				lnp$eFG5[i] <- vals[5,2]
	}else{
	}
}

for(i in 1:nrow(lnp)){   # 
	ast <- c(indi$AST.[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$AST.[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$AST.[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$AST.[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$AST.[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	tov <- c(indi$TOV.[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$TOV.[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$TOV.[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$TOV.[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$TOV.[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])	

	if(length(ast) == 5 & length(tov) == 5){
		vals <- data.frame(ast, tov)
		vals <- vals[order(vals$ast, decreasing = T), ]		
			lnp$AST1[i] <- vals[1,1]
				lnp$TOV1[i] <- vals[1,2]	
			lnp$AST2[i] <- vals[2,1]
				lnp$TOV2[i] <- vals[2,2]			
			lnp$AST3[i] <- vals[3,1]
				lnp$TOV3[i] <- vals[3,2]	
			lnp$AST4[i] <- vals[4,1]
				lnp$TOV4[i] <- vals[4,2]	
			lnp$AST5[i] <- vals[5,1]
				lnp$TOV5[i] <- vals[5,2]	

	}else{
	}
}

for(i in 1:nrow(lnp)){   # 
	orb <- c(indi$ORB.[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$ORB.[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$ORB.[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$ORB.[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$ORB.[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	if(length(orb) == 5){
		vals <- as.numeric(c(orb))
		vals <- sort(vals, decreasing = T)		
			lnp$ORB1[i] <- vals[1]
			lnp$ORB2[i] <- vals[2]
			lnp$ORB3[i] <- vals[3]
			lnp$ORB4[i] <- vals[4]
			lnp$ORB5[i] <- vals[5]
	}else{
	}
}

for(i in 1:nrow(lnp)){   # 
	ft <- c(indi$FT[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$FT[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$FT[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$FT[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$FT[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	if(length(orb) == 5){
		vals <- as.numeric(c(ft))
		vals <- sort(vals, decreasing = T)		
			lnp$FT1[i] <- vals[1]
			lnp$FT2[i] <- vals[2]
			lnp$FT3[i] <- vals[3]
			lnp$FT4[i] <- vals[4]
			lnp$FT5[i] <- vals[5]
	}else{
	}
}

for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$MIDA[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$MIDA[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$MIDA[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$MIDA[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$MIDA[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$MIDA[i] <- vals[1] + vals[2] + vals[3] + vals[4] + vals[5]
	}else{
	}
}

for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$RIMA[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$RIMA[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$RIMA[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$RIMA[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$RIMA[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$RIMA[i] <- vals[1] + vals[2] + vals[3] + vals[4] + vals[5]
	}else{
	}
}

for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$X3PA[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$X3PA[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$X3PA[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$X3PA[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$X3PA[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$X3PA[i] <- vals[1] + vals[2] + vals[3] + vals[4] + vals[5]
	}else{
	}
}



for(i in 1:nrow(lnp)){   # 
	vals <- c(indi$ASD[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season]*indi$FGA[lnp$Name1[i] == indi$Name & lnp$Season[i] == indi$Season], indi$ASD[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season]*indi$FGA[lnp$Name2[i] == indi$Name & lnp$Season[i] == indi$Season], indi$ASD[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season]*indi$FGA[lnp$Name3[i] == indi$Name & lnp$Season[i] == indi$Season], indi$ASD[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season]*indi$FGA[lnp$Name4[i] == indi$Name & lnp$Season[i] == indi$Season], indi$ASD[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season]*indi$FGA[lnp$Name5[i] == indi$Name & lnp$Season[i] == indi$Season])
	vals <- sort(vals, decreasing = T)
	if(length(vals == 5)){
		lnp$ASD[i] <- vals[1] + vals[2] + vals[3] + vals[4] + vals[5]
	}else{
	}
}

#######



m1 <- lm(scale(FFO.efg) ~ scale(eFG1) + scale(eFG2) + scale(eFG3) + scale(eFG4) + scale(eFG5) + scale(X3PA) + scale(MIDA) + scale(RIMA) + scale(AST) + scale(I(ATT1 + ATT2 + ATT3 + ATT4 + ATT5)), data = lnp, weights = MP)
summary(m1)



m1 <- lm(FFO.efg ~ eFG1 + eFG2 + eFG3 + eFG4 + eFG5 + X3PA + MIDA + RIMA + AST + I(ATT1 + ATT2 + ATT3 + ATT4 + ATT5), data = lnp, weights = MP)
summary(m1)

m1 <- lm(FFO.efg ~ O.eFG + eFG1 + eFG2 + eFG3 + eFG4 + eFG5 + I(ASD/(AST1 + AST2 + AST3 + AST4 +AST5)) + I(X3PA/(RIMA + MIDA)), data = lnp, weights = MP)
summary(m1)

m1 <- lm(FFO.reb ~ ORB1 + ORB2 + ORB3 + ORB4 + ORB5, data = lnp, weights = MP)
summary(m1)

m1 <- lm(FFD.reb ~ DRB1 + DRB2 + DRB3 + DRB4 + DRB5, data = lnp, weights = MP)

m1 <- lm(FFD.reb ~ I(DRB1 + DRB2 + DRB3 + DRB4 + DRB5) + DRB1 + DRB5, data = lnp, weights = MP)


summary(m1)


m1 <- lm(FFO.efg ~ eFG1 + eFG2 + eFG3 + eFG4 + eFG5 + X3PA + MIDA + RIMA + I(AST1 + AST2 + AST3 + AST4 + AST5) + I(ATT1 + ATT2 + ATT3 + ATT4 + ATT5), data = lnp, weights = MP)
m1 <- lm(FFO.tov ~ I(AST1 + AST2 + AST3 + AST4 + AST5) + I(TOV1 + TOV2 + TOV3 + TOV4 + TOV5) + X3PA + RIMA, data = lnp, weights = MP)
m1 <- lm(FFO.reb ~ ORB1 + ORB2 + ORB3 + ORB4 + ORB5 + X3PA + MIDA + RIMA, data = lnp, weights = MP)
m1 <- lm(FFO.ftr ~ I(FT1 + FT2 + FT3 + FT4 + FT5) + X3PA + MIDA + RIMA + I(AST1 + AST2 + AST3 + AST4 + AST5), data = lnp, weights = MP)



#########
oefg <- lm(scale(FFO.efg) ~ scale(O.eFG) + scale(eFG1) + scale(eFG2) + scale(eFG3) + scale(eFG4) + scale(eFG5) + scale(I(ASD/AST1)) + scale(I(X3PA/(X3PA + RIMA + MIDA))), data = lnp, weights = MP)
#summary(oefg)
#
otov <- lm(scale(FFO.tov) ~ scale(O.TOV) + scale(TOV5) + scale(I(TOV1 + TOV2 + TOV3 + TOV4)) + scale(I(AST1 + AST2 + AST3 + AST4 + AST5)) + scale(I(ATT1 + ATT2 + ATT3 + ATT4 + ATT5)), data = lnp, weights = MP)
#summary(otov)
#
oreb <- lm(scale(FFO.reb) ~ scale(O.REB) + scale(ORB1) + scale(ORB2) + scale(ORB3) + scale(ORB4) + scale(ORB5) + scale(X3PA), data = lnp, weights = MP)
#summary(oreb)
#
oftr <- lm(scale(FFO.ftr) ~ scale(O.FT) + scale(I(FT1 + FT2 + FT3 + FT4 + FT5)) + scale(I(ATT1 + ATT2 + ATT3 + ATT4 + ATT5)) + scale(X3PA) + scale(MIDA) + scale(RIMA), data = lnp, weights = MP)
#summary(oftr)
########

lnp$oefg <- round(as.numeric(predict(oefg, newdata = lnp)), 2)
lnp$otov <- round(as.numeric(predict(otov, newdata = lnp)), 2)
lnp$oreb <- round(as.numeric(predict(oreb, newdata = lnp)), 2)
lnp$oftr <- round(as.numeric(predict(oftr, newdata = lnp)), 2)




