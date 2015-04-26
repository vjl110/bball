rm(list=ls())
setwd("~/GitHub/bball/data")

tm <- read.csv("NCAA_team.csv", strip.white = T)
	tm <- subset(tm, Season >= 1990)
	tm15 <- read.csv("ncaa.tm_2015upd.csv", strip.white=T)
tm <- rbind(tm, tm15)	


# Minutes
tm$MP[is.na(tm$MP)] <- tm$G[is.na(tm$MP)]*200

# Rebounding
tm$ORB[is.na(tm$ORB)] <- tm$TRB[is.na(tm$ORB)] - tm$DRB[is.na(tm$ORB)]
tm$DRB[is.na(tm$DRB)] <- tm$TRB[is.na(tm$DRB)] - tm$ORB[is.na(tm$DRB)]
tm$TRB[is.na(tm$TRB)] <- tm$ORB[is.na(tm$TRB)] + tm$DRB[is.na(tm$TRB)]

# Scoring
tm$FG[is.na(tm$FG)] <- tm$X2P[is.na(tm$FG)] + tm$X3P[is.na(tm$FG)]
tm$FGA[is.na(tm$FGA)] <- tm$X2PA[is.na(tm$FGA)] + tm$X3PA[is.na(tm$FGA)]
tm$X2P[is.na(tm$X2P)] <- tm$FG[is.na(tm$X2P)] - tm$X3P[is.na(tm$X2P)]
tm$X2PA[is.na(tm$X2PA)] <- tm$FGA[is.na(tm$X2PA)] - tm$X3PA[is.na(tm$X2PA)]
tm$X3P[is.na(tm$X3P)] <- tm$FG[is.na(tm$X3P)] - tm$X2P[is.na(tm$X3P)]
tm$X3PA[is.na(tm$X3PA)] <- tm$FGA[is.na(tm$X3PA)] - tm$X2PA[is.na(tm$X3PA)]
tm$PTS[is.na(tm$PTS)] <- tm$FT[is.na(tm$PTS)] + tm$X2P[is.na(tm$PTS)]*2 + tm$X3P[is.na(tm$PTS)]*3

# Pace
tm$PACE <- ((tm$FGA + 0.475*tm$FTA + tm$TOV - tm$ORB) / tm$MP)*5

# Impute Pace
for(i in 1:nrow(tm)){
	tm$PACE[i] <- ifelse(is.na(tm$PACE[i]), 
		weighted.mean(tm$PACE[tm$Coach == tm$Coach[i]], 1/abs(tm$Season[i] - tm$Season[tm$Coach == tm$Coach[i]]), na.rm=T), 
			tm$PACE[i])
}
for(i in 1:nrow(tm)){
	tm$PACE[i] <- ifelse(is.na(tm$PACE[i]), 
		mean(tm$PACE[tm$Season == tm$Season[i]], na.rm=T), 
			tm$PACE[i])
}


# per100poss
tm$POSS <- tm$MP*tm$PACE/5
for(i in 18:34){
	tm[,i] <- round((tm[,i]/tm$POSS)*100, 1)
}

# impute empties
for(j in 18:34){
	tm[is.na(tm[,j]), j] <- mean(tm[ ,j], na.rm=T)
}

# Save

tm <- data.frame('Tm' = tm$Tm, 'Season' = tm$Season, 'tmX2P' = tm$X2P, 'tmX2PA' = tm$X2PA, 'tmX3P' = tm$X3P, 'tmX3PA' = tm$X3PA, 'tmFT' = tm$FT, 'tmFTA' = tm$FTA, 'tmORB' = tm$ORB, 'tmTRB' = tm$TRB, 'tmAST' = tm$AST, 'tmTOV' = tm$TOV, 'tmSTL' = tm$STL, 'tmBLK' = tm$BLK, 'tmPF' = tm$PF, 'tmPTS' = tm$PTS, 'PACE' = tm$PACE, 'MOV' = tm$SRS - tm$SOS, 'SOS' = tm$SOS, 'Coach' = tm$Coach)
  
save(tm, file = "ncaa_tm")




