rm(list=ls())
setwd("C:/Users/Layne/Dropbox/bball")

##########################
#### NCAA TEAM ###########
##########################

tm <- read.csv("data/NCAA_team.csv", strip.white = T)
	tm <- subset(tm, Season >= 1990)
	tm15 <- read.csv("data/ncaa.tm_2015upd.csv", strip.white=T)
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
  
save(tm, file = "data/ncaa_tm")

##########################
#### NCAA INDI ###########
##########################

# Read in data
ncaa <- read.csv("data/NCAA_indi.csv", strip.white = T)
ncaa <- subset(ncaa, Season >= 1990)
	ncaa15 <- read.csv("data/ncaa.ind_2015upd.csv", strip.white = T)
	ncaa$Name <- as.character(ncaa$Name)
	ncaa15$Name <- as.character(ncaa15$Name)	
	for(i in 1:nrow(ncaa15)){
		ncaa15$EXP[i] <- ifelse(length(ncaa$EXP[ncaa$Name == ncaa15$Name[i]]) > 0, max(ncaa$EXP[ncaa$Name == ncaa15$Name[i]], na.rm=T) + 1, 1)
	}	
ncaa <- rbind(ncaa, ncaa15)	
load("data/nba")
load("data/ncaa_tm")

####  combine

ncaa <- merge(ncaa, tm, by = c('Tm', 'Season'), all.x = T) 
ncaa <- merge(ncaa, nba, by = c('Name'), all.x = T) 

# per100poss
ncaa$POSS <- ncaa$MP*ncaa$PACE
for(i in 7:20){
	ncaa[,i] <- round((ncaa[,i]/ncaa$POSS)*100, 1)
}

ncaa$X2P[is.na(ncaa$X2P)] <- 0
ncaa$X2PA[is.na(ncaa$X2PA)] <- 0
ncaa$X3P[is.na(ncaa$X3P)] <- 0
ncaa$X3PA[is.na(ncaa$X3PA)] <- 0

###

bio <- read.csv("data/BBRbio.csv", strip.white = T)
ncaa <- merge(ncaa, bio, by = "Name", all.x=T)
ncaa$Age <- ((ncaa$Season - 1900)*365 - ncaa$DOB)/365

######  Save out

save(ncaa, file = "data/ncaa_indi")


###################################
#### EURO INDI AND TEAM ###########
###################################

###
###### Arrange euro data then combine with WS/RAPM and other details.
###

# ARRANGE EURO DATA
euro <- read.csv("data/INTL_indi.csv", strip.white = T)
	euro15 <- read.csv("data/euro.ind_2015upd.csv", strip.white = T)
euro <- rbind(euro, euro15)
tm <- read.csv("data/INTL_team.csv")
	tm15 <- read.csv("data/euro.tm_2015upd.csv", strip.white = T)
tm <- rbind(tm, tm15)
	
euro <- subset(euro, League != "FIBAEU22")

# append team data to individuals
euro <- merge(euro, tm, by = c("Season", "League", "Team"), all.x=T)

## impute ORBs for NAs and tranform TRB into DRB
ORB.prj <- mean(euro$ORB[euro$MP > 200]/euro$TRB[euro$MP > 200], na.rm=T)
euro$ORB[is.na(euro$ORB) & !is.na(euro$TRB)] <- euro$TRB[is.na(euro$ORB) & !is.na(euro$TRB)]*ORB.prj
euro$ORB_tm[is.na(euro$ORB_tm) & !is.na(euro$TRB_tm)] <- euro$TRB_tm[is.na(euro$ORB_tm) & !is.na(euro$TRB_tm)]*ORB.prj
euro$TRB <- euro$TRB - euro$ORB
euro$TRB_tm <- euro$TRB_tm - euro$ORB_tm
colnames(euro)[16] <- "DRB"
colnames(euro)[32] <- "DRB_tm"

# Impute missing pace
euro$Pace[is.na(euro$Pace)] <- mean(tm$Pace, na.rm=T)

# Convert to per100poss
euro$MP_tm[is.na(euro$MP_tm)] <- euro$G_tm[is.na(euro$MP_tm)]*40*5
for(i in 25:38){
	Poss <- (euro$MP_tm * euro$Pace)/5
	euro[,i] <- ifelse(!is.na(euro[,i]), round((euro[,i]/Poss)*100, 1), NA)
}
### INDIVIDUAL STAS per100
for(i in 9:22){
	Poss <- euro$MP * euro$Pace
	euro[,i] <- (euro[,i]/Poss)*100
}

######  Standardize to EL 
lgs <- c("ACB", "GREEK", "ADRIATIC", "EUROCUP", "FRENCH", "ITALIAN", "FIBAEU")
eu.adj <- as.data.frame(matrix(nrow = length(lgs), ncol = 16)) 
colnames(eu.adj) <- c("N", "MPG", "X2P", "X2PA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "AST", "TOV", "STL", "BLK", "PF", "PTS") 
rownames(eu.adj) <- lgs
	put <- 1
for(i in lgs){
	LG <- subset(euro, League == i)
	EL <- subset(euro, League == "EURO")
		cmb <- merge(LG, EL, by = c("Name", "Season"))
		eu.adj[put,1] <- nrow(cmb)
		for(j in 8:22){
			cmb2 <- subset(cmb, cmb[,j] > 0 & cmb[,j+37] >0)
			eu.adj[put,(j-6)] <- weighted.mean(cmb2[, j+37], ifelse(cmb2$MP.x > cmb2$MP.y, cmb2$MP.y, cmb2$MP.x), na.rm=T)/weighted.mean(cmb2[, j], ifelse(cmb2$MP.x > cmb2$MP.y, cmb2$MP.y, cmb2$MP.x), na.rm=T)
		}	
		put <- put + 1
}
##  Applying within Euro translation to seniors
put <- 1
for(i in lgs){
	for(j in 2:ncol(eu.adj)){	
		euro[euro$League == i, j+6] <-  euro[euro$League == i, j+6]*eu.adj[put, j]
	}
	for(j in 3:ncol(eu.adj)){	
		euro[euro$League == i, j+22] <-  euro[euro$League == i, j+22]*eu.adj[put, j]
	}	
	put <- put + 1
}

# Global seniors adjustment
lgs <- c("OLYMP", "WC", "FIBAMER")
gl.adj <- as.data.frame(matrix(nrow = length(lgs), ncol = 16)) 
colnames(gl.adj) <- c("N", "MPG", "X2P", "X2PA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "AST", "TOV", "STL", "BLK", "PF", "PTS") 
rownames(gl.adj) <- lgs
	put <- 1
for(i in lgs){
	LG <- subset(euro, League == i)
	EL <- subset(euro, (League == "EURO" | League == "EUROCUP" | League == "ACB" | League == "FRENCH" | League == "GREEK" | League == "ITALIAN" | League == "ADRIATIC" | League == "FIBAEU"))
		cmb <- merge(LG, EL, by = c("Name", "Season"))
		gl.adj[put,1] <- nrow(cmb)
		for(j in 8:22){
			cmb2 <- subset(cmb, cmb[,j] > 0 & cmb[,j+37] >0)
			gl.adj[put,(j-6)] <- weighted.mean(cmb2[, j+37], ifelse(cmb2$MP.x > cmb2$MP.y, cmb2$MP.y, cmb2$MP.x), na.rm=T)/weighted.mean(cmb2[, j], ifelse(cmb2$MP.x > cmb2$MP.y, cmb2$MP.y, cmb2$MP.x), na.rm=T)
		}	
		put <- put + 1
}
##  Applying within Global translation
put <- 1
for(i in lgs){
	for(j in 2:ncol(gl.adj)){	
		euro[euro$League == i, j+6] <-  euro[euro$League == i, j+6]*gl.adj[put, j]
	}
	for(j in 3:ncol(gl.adj)){	
		euro[euro$League == i, j+22] <-  euro[euro$League == i, j+22]*gl.adj[put, j]
	}	
	put <- put + 1
}

## finding age-group translations
lgs <- c("FIBAEU18", "FIBAEU20", "WC19")
dv.adj <- as.data.frame(matrix(nrow = length(lgs), ncol = 16)) 
colnames(dv.adj) <- c("N", "MPG", "X2P", "X2PA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "AST", "TOV", "STL", "BLK", "PF", "PTS") 
rownames(dv.adj) <- lgs
	put <- 1
for(i in lgs){
	LG <- subset(euro, League == i)
	EL <- subset(euro, League != "FIBAEU18" & League != "FIBAEU20" & League != "WC19")
		cmb <- merge(LG, EL, by = c("Name", "Season"))
		dv.adj[put,1] <- nrow(cmb)
		for(j in 8:22){
			cmb2 <- subset(cmb, cmb[,j] > 0 & cmb[,j+37] >0)
			dv.adj[put,(j-6)] <- weighted.mean(cmb2[, j+37], ifelse(cmb2$MP.x > cmb2$MP.y, cmb2$MP.y, cmb2$MP.x), na.rm=T)/weighted.mean(cmb2[, j], ifelse(cmb2$MP.x > cmb2$MP.y, cmb2$MP.y, cmb2$MP.x), na.rm=T)
		}	
		put <- put + 1
}
##  Applying within age-groups translation to seniors
put <- 1
for(i in lgs){
	for(j in 2:ncol(dv.adj)){	
		euro[euro$League == i, j+6] <-  euro[euro$League == i, j+6]*dv.adj[put, j]
	}
	for(j in 3:ncol(dv.adj)){	
		euro[euro$League == i, j+22] <-  euro[euro$League == i, j+22]*dv.adj[put, j]
	}	
	put <- put + 1
}
####

# Impute mean values for those without team data (or with prior adjustments)
for(i in 25:38){
	euro[,i] <- ifelse(!is.na(euro[,i]), euro[,i], mean(euro[,i], na.rm=T))
}

### impute blks and PFs
for(i in 1:nrow(euro)){
	euro$BLK[i] <- ifelse(is.na(euro$BLK[i]), weighted.mean(euro$BLK[euro$Name == euro$Name[i]], euro$MP[euro$Name == euro$Name[i]], na.rm=T), euro$BLK[i])
	euro$PF[i] <- ifelse(is.na(euro$PF[i]), weighted.mean(euro$PF[euro$Name == euro$Name[i]], euro$MP[euro$Name == euro$Name[i]], na.rm=T), euro$PF[i])
}

###

euro <- euro[-c(23:24)]


# Merge within seasons...
library(plyr)
eu.inf <- euro[c(1:4)]
eu.sum <- euro[-c(2:3, 5, 8:39)]
eu.mean <- euro[-c(2:3, 5:6)]

sum.hld <- aggregate( . ~ Name + Season, data = eu.sum, sum)
mn.hld <- ddply(eu.mean, .(Name, Season), function(df) {
   numcolwise(weighted.mean)(df, na.rm = T, w=df$MP)
})
mn.hld <- mn.hld[-3]

euro <- merge(sum.hld, mn.hld, by = c("Name", "Season"))

####
####  Add in other information!
nba$OBS[nba$Name == "Arvydas Sabonis"] <- 11.14
nba2 <- nba[-2]
euro <- merge(euro, nba2, by = "Name", all.x=T)

#### 
bio <- read.csv("data/BBRbio.csv", strip.white = T)
euro <- merge(euro, bio, by="Name", all.x=T)
euro$Age <- ((euro$Season-1900)*365 - euro$DOB)/365


save(euro, file = "data/euro")
save(eu.inf, file = "data/eu.inf")











