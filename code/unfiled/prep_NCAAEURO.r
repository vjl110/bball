rm(list=ls())
setwd("C:/Users/Layne/Dropbox/Archive/BBall/Draft proj")
#setwd("C:/Users/SCAN Project/Dropbox/Archive/BBall/Draft proj")

load("nba")

###
###### Arrange College data then combine with WS/RAPM and other details.
###

# ARRANGE NCAA DATA
BR <- read.csv("BBRncaa.csv", strip.white = T)
DX <- read.csv("DXncaa.csv", strip.white = T)

ncaa <- merge(BR, DX, by=c("Name", "Season"), all.x=T)
ncaa <- ncaa[-c(5)]
for(i in 5:19){
	ncaa[,i] <- ifelse(!is.na(ncaa[,i+16]), ncaa[,i+16], ncaa[,i])
}  
ncaa <- ncaa[order(ncaa[,2], decreasing=T),]
ncaa <- ncaa[-c(21:36)]
for(i in 6:19){
	ncaa[,i] <- round((ncaa[,i]/ncaa[,5])*40, digits=2)
}

ncaa$X2P[is.na(ncaa$X2P)] <- 0
ncaa$X2PA[is.na(ncaa$X2PA)] <- 0
ncaa$X3P[is.na(ncaa$X3P)] <- 0
ncaa$X3PA[is.na(ncaa$X3PA)] <- 0

SCHOOL <- read.csv("School.stats.csv", strip.white = T)
SCHOOL <- SCHOOL[-c(1, 4:7, 10:13)]
SCHOOL$MOV <- SCHOOL$SRS - SCHOOL$SOS
ncaa <- merge(ncaa, SCHOOL, by=c("Team", "Season"), all.x=T)
ncaa$SOS[is.na(ncaa$SOS)] <- -15
ncaa$MOV[is.na(ncaa$MOV)] <- 0

####

#### merge then figure out position
ncaa <- merge(ncaa, nba, by="Name", all.x=T)
ncaa$Pos <- round(ifelse(!is.na(ncaa$Pos), ncaa$Pos, ncaa$Pos.nba), digits=2)
ncaa <- ncaa[-c(25)]  #### set to whatever nba pos is...
bio <- read.csv("BBRbio.csv", strip.white = T)
ncaa <- merge(ncaa, bio, by="Name", all.x=T)
ncaa$Pos[is.na(ncaa$Pos) & ncaa$Height < 75] <- 1
ncaa$Pos[is.na(ncaa$Pos) & ncaa$Height >= 75 & ncaa$Height < 78] <- 2
ncaa$Pos[is.na(ncaa$Pos) & ncaa$Height >= 78 & ncaa$Height < 81] <- 3
ncaa$Pos[is.na(ncaa$Pos) & ncaa$Height >= 81 & ncaa$Height < 83] <- 4
ncaa$Pos[is.na(ncaa$Pos) & ncaa$Height >= 83] <- 5

ncaa$Age <- ((ncaa$Season-1900)*365 - ncaa$DOB)/365

# Add physical variables
phyz <- read.csv("phyz.csv", strip.white = T)
phyz <- phyz[-2]
phyz <- phyz[order(phyz$Wingspan),]
phyz <- phyz[order(phyz$NS_vert),]
phyz <- subset(phyz, !duplicated(Name))


ncaa <- merge(ncaa, phyz, by="Name", all.x=T)
ncaa$Height <- ifelse(!is.na(ncaa$inches), ncaa$inches, ncaa$Height)
ncaa$Weight <- ifelse(!is.na(ncaa$lbs), ncaa$lbs, ncaa$Weight)
ncaa <- ncaa[-c(ncol(ncaa), (ncol(ncaa) - 1))]

save(ncaa, file="ncaa")

####
###	EURO TIME!!!
####

###
###### Arrange euro data then combine with WS/RAPM and other details.
###

# ARRANGE EURO DATA
euro <- read.csv("GLOBAL_stats_6sept2014.csv", strip.white = T)
tm <- read.csv("INTERNATIONAL_TEAM_6sept2014.csv")

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


pro <- subset(eu.inf, (League=="EURO"|League=="EUROCUP"|League=="ACB"|League=="FRENCH"|League=="GREEK"|League=="ITALIAN"|League== "ADRIATIC"))
nat <- subset(eu.inf, League!="EURO"&League!="EUROCUP"&League!="ACB"&League!="FRENCH"&League!="GREEK"&League!="ITALIAN"&League!= "ADRIATIC")
for(i in 1:nrow(euro)){
	pro.hld <- pro$Team[pro$Name == euro$Name[i] & pro$Season == euro$Season[i]]
	nat.hld <- nat$Team[nat$Name == euro$Name[i] & nat$Season == euro$Season[i]]
	euro$Team.pro[i] <- ifelse(length(pro.hld) > 0, as.character(pro.hld[1]), NA)
	euro$Team.nat[i] <- ifelse(length(nat.hld) > 0, as.character(nat.hld[1]), NA)
}



####
####  Add in other information!
nba$OBS[nba$Name == "Arvydas Sabonis"] <- 11.14
nba2 <- nba[-2]
euro <- merge(euro, nba2, by = "Name", all.x=T)

#### 
bio <- read.csv("BBRbio.csv", strip.white = T)
euro <- merge(euro, bio, by="Name", all.x=T)
euro$Age <- ((euro$Season-1900)*365 - euro$DOB)/365


### impute blks and PFs
for(i in 1:nrow(euro)){
	euro$BLK[i] <- ifelse(is.na(euro$BLK[i]), weighted.mean(euro$BLK[euro$Name == euro$Name[i]], euro$MP[euro$Name == euro$Name[i]], na.rm=T), euro$BLK[i])
	euro$PF[i] <- ifelse(is.na(euro$PF[i]), weighted.mean(euro$PF[euro$Name == euro$Name[i]], euro$MP[euro$Name == euro$Name[i]], na.rm=T), euro$PF[i])
}

save(euro, file = "euro")





