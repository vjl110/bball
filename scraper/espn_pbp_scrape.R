setwd("~/GitHub/bball")

library(XML)
library(RCurl)
library(httr)
library(stringr)

###
	GAMESET <- read.csv("GAMESET.csv", stringsAsFactors=FALSE)
	LINEUP.DMP <- read.csv("LINEUP_DMP.csv", stringsAsFactors=FALSE)
	PLAYER.DMP <- read.csv("PLAYER_DMP.csv", stringsAsFactors=FALSE)
##

###
##### Build "GAMESET" of Game-IDs to pull data from.
###
GAMESET <- data.frame("YEAR" = NA, "MONTH" = NA, "DAY" = NA, "GM1" = NA, "GM2" = NA, "GM3" = NA, "GM4" = NA, "GM5" = NA, "GM6" = NA, "GM7" = NA, "GM8" = NA, "GM9" = NA, "GM10" = NA, "GM11" = NA, "GM12" = NA, "GM13" = NA, "GM14" = NA, "GM15" = NA)
year <- 2000:2015
mnth <- 1:12
day <- 1:31
for(y in 1:length(year)){
	for(m in 1:length(mnth)){
		for(d in 1:length(day)){			
			ngt <- paste0(c("http://www.basketball-reference.com/boxscores/index.cgi?month=", mnth[m], "&day=", day[d], "&year=", year[y]), collapse = "")
			ngt <- GET(ngt)
			ngt <- content(ngt, as="text")
			lnk <- xpathSApply(htmlParse(ngt), "//a/@href")
			lnk <- as.character(lnk[grep("boxscores/pbp/", lnk)])
			lnk <- c(year[y], mnth[m], day[d], lnk)
			blank <- ncol(GAMESET) - length(lnk)
			GAMESET <- rbind(GAMESET, c(lnk, rep(NA, blank)))
		}
	}
}
GAMESET <- subset(GAMESET, !is.na(GM1))	
#write.csv(GAMESET, "GAMESET.csv", row.names=FALSE)

###
##### Work through Game-IDs to produce PbP data.
###

#######  Pull specific games from the gameset and start working on it...
#GAMESET <- read.csv("GAMESET.csv")

LINEUP.DMP <- data.frame("YEAR"=NA, "MONTH"=NA, "DAY"=NA, "Tm"=NA, "Opp"=NA, "HOME"=NA, "LINEUP"=NA, "CP_LINEUP"=NA, "POSS"=NA, "POSS.1"=NA, "FGA_O"=NA, "POSS.to_O"=NA, "RB.chance_O"=NA, "FGA_D"=NA, "POSS.to_D"=NA, "RB.chance_D"=NA, "EFG_O"=NA, "TOV_O"=NA, "REB_O"=NA, "FT_O"=NA, "EFG_D"=NA, "TOV_D"=NA, "REB_D"=NA, "FT_D"=NA)
PLAYER.DMP <- data.frame("Name.full" = NA, "Name.shrt" = NA, "ID" = NA, "HOME" = NA)

for(n in 2935:nrow(GAMESET)){
#for(n in 1:nrow(GAMESET)){
	for(g in 4:length(GAMESET[n,][!is.na(GAMESET[n,])])){	
		GM.hld <- GAMESET[n, c(1:3, g)]
#######

pbp.url <-paste(c("http://www.basketball-reference.com/", GM.hld[4]), collapse = "")
	pbp <- GET(pbp.url)
	pbp <- readHTMLTable(rawToChar(pbp$content), stringsAsFactors = F)
	pbp <- as.data.frame(pbp[[length(pbp)]])
		pbp[pbp == "Â"] <-  NA
	teams <- as.character(pbp[1, c(2,4)])
		pbp <- pbp[c(2,6)]
		pbp <- pbp[-1,]
		colnames(pbp) <- c(teams)

box.url <- gsub("/pbp", "", pbp.url)	# using the box to build a list of players who should be in game.
	box <- GET(box.url)
			IDs <- content(box, as="text")	#grab unique IDs
			IDs <- xpathSApply(htmlParse(IDs), "//a/@href")
			IDs <- unique(as.character(IDs[grep("players/", IDs)]))[-1]
			IDs <- gsub("players", "", IDs)
			IDs <- gsub("html", "", IDs)			
			IDs <- gsub("[^A-Za-z0-9]", "", IDs)
	box <- readHTMLTable(rawToChar(box$content), stringsAsFactors = F)
	away.bx <- as.data.frame(box[[length(box) - 3]])
		players.away <- subset(away.bx, MP != "MP")[1]
		colnames(players.away)[1] <- "Name.full"
		for(i in 1:nrow(players.away)){
			players.away$Name.shrt[i] <- gsub("^.*? ", paste0(substring(players.away$Name.full[i], 1, 1), ". "), players.away$Name.full[i])
		}
	home.bx <- as.data.frame(box[[length(box) - 2]])
		players.home <- subset(home.bx, MP != "MP")[1]
		colnames(players.home)[1] <- "Name.full"
		for(i in 1:nrow(players.home)){
			players.home$Name.shrt[i] <- gsub("^.*? ", paste0(substring(players.home$Name.full[i], 1, 1), ". "), players.home$Name.full[i])
		}
		IDs <- IDs[1:length(c(players.away$Name.full, players.home$Name.full))]
	players <- data.frame("Name.full" = c(players.away$Name.full, players.home$Name.full), "Name.shrt" = c(players.away$Name.shrt, players.home$Name.shrt), 
			      "ID" = IDs, "HOME" = c(rep( 0, nrow(players.away)), rep( 1, nrow(players.home))) )

	if(length(unique(players$Name.shrt[players$HOME == 0])) < length(players$Name.shrt[players$HOME == 0]) | length(unique(players$Name.shrt[players$HOME == 1])) < length(players$Name.shrt[players$HOME == 1])){
	}else{


# Use while loops to split X enters game to replace Y such that each name falls on the correct side of the stint split
while(length(grep(" enters", pbp[, 1])) > 0){
	ntr <- as.numeric(grep(" enters", pbp[, 1])[1])
	chng <- unlist(strsplit(pbp[ntr, 1], " enters"))
	pbp <- rbind(pbp[1:(ntr-1), ], 
		     c(chng[2], pbp[ntr,2]), 
		     c(paste("enter", chng[1]), pbp[ntr,2]), 
		     pbp[(ntr+1):nrow(pbp), ])
}
#
while(length(grep(" enters", pbp[, 2])) > 0){
	ntr <- as.numeric(grep(" enters", pbp[, 2])[1])
	chng <- unlist(strsplit(pbp[ntr, 2], " enters"))
	pbp <- rbind(pbp[1:(ntr-1), ], 
		     c(pbp[ntr,1], chng[2]), 
		     c(pbp[ntr,1], paste("enter", chng[1])), 
		     pbp[(ntr+1):nrow(pbp), ])
}

## Add quarter indicator
	row.names(pbp) <- 1:nrow(pbp)	
	QTR <- c(1, grep("Start of", pbp[2:(nrow(pbp) -1), 1]), nrow(pbp)) 
	quarter <- 1
	for(i in QTR){
		pbp$Q[as.numeric(row.names(pbp)) > i] <- quarter
		quarter <- quarter + 1
	}

# VERY SPECIFIC FIXES FOR BAD PBP
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/200212260SEA.html"){
		pbp <- pbp[-c(nrow(pbp), (nrow(pbp) -1)),]
	}else{
	}
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/200312210DET.html"){
		pbp$Utah[101] <- NA
	}else{
	}	
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/200111010ATL.html"){
		pbp <- pbp[-c(412:413),]
	}else{
	}	
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/200203260IND.html"){
		pbp <- pbp[-c(458:459, 467:468),]
	}else{
	}
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/200511130BOS.html"){
		pbp <- pbp[-c(228:229),]
	}else{
	}	
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/200904150MIA.html"){
		pbp <- pbp[-c(111),]
	}else{
	}	
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/200911110NJN.html"){
		pbp <- pbp[-c(469:470),]
	}else{
	}
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/201004130PHO.html"){
		pbp$Denver[148] <- gsub("A. Afflalo", "C. Anthony", pbp$Denver[148])
		pbp <- pbp[-c(150:151),]
	}else{
	}
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/201012230SAC.html"){
		pbp <- pbp[-c(507:508),]
	}else{
	}	
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/201303080DET.html"){
		pbp <- pbp[-c(499:500, 512:513),]
	}else{
	}	
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/201304100CLE.html"){
		pbp <- pbp[-c(490:491, 503:504),]
	}else{
	}	
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/201311270DAL.html"){
		pbp <- pbp[-c(422:423),]
	}else{
	}	
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/201312040NOP.html"){
		pbp$Dallas[112] <- gsub("G. Mekel", "", pbp$Dallas[112])
	}else{
	}
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/201312250SAS.html"){
		pbp <- pbp[-c(102:103),]
	}else{
	}	
	if(pbp.url == "http://www.basketball-reference.com//boxscores/pbp/201404210OKC.html"){
		pbp <- pbp[-c(507:508,513:514),]
	}else{
	}	
############################################


#############  BUILD FOR AWAY TEAM

	#build and clean a dataframe to plug in players from team 1
	player.mtx <- matrix(nrow = nrow(pbp), ncol = nrow(subset(players, HOME == 0)))
	tm1.gm <- data.frame(pbp, player.mtx)
	colnames(tm1.gm) <-c(colnames(pbp), as.character(subset(players, HOME == 0)$Name.shrt))


# VERY SPECIFIC FIXES FOR BAD PBP
	if(n == 36 & g == 6){
	       tm1.gm[199, 1] <- gsub("C. Alexander", "H. Davis", tm1.gm[199, 1])	
	       tm1.gm[200, 1] <- gsub("H. Davis", "C. Alexander", tm1.gm[200, 1])
	}else{
	}

############################################	

	tm1.gm[,1] <- gsub("$", " ", tm1.gm[,1])
	tm1.gm[,2] <- gsub("$", " ", tm1.gm[,2])

	for(j in 4:ncol(tm1.gm)){
		NAME <- paste(colnames(tm1.gm)[j], "")
		if(length(unique(players$Name.shrt)) == length(players$Name.shrt)){
			mentions <- c(grep(NAME, tm1.gm[, 1]), grep(NAME, tm1.gm[, 2]))
		}else{
			mentions <- c(grep(NAME, tm1.gm[, 1]))
			overlaps <- sort(c(grep(paste("foul|block|steal by|three seconds by", NAME), tm1.gm[, 1]), grep(paste(NAME, "gains possession", sep=""), tm1.gm[, 1]), grep("Jump ball", tm1.gm[, 1])))
			if(length(overlaps) > 0){
				mentions <- sort(mentions[!(mentions %in% overlaps)])
			}			
		}
			offcourt <- c(grep("Technical|ejected", tm1.gm[, 1], ignore.case=T), grep("Technical|ejected", tm1.gm[, 2], ignore.case=T))
			if(length(offcourt) > 0){
				mentions <- sort(mentions[!(mentions %in% offcourt)])
			}		
		entr <- grep(paste("enter", NAME), tm1.gm[,1])
		exit <- grep(paste("the game for", NAME), tm1.gm[,1])
		EVNTS <- as.data.frame(matrix(nrow = length(c(entr, exit, QTR)), ncol = 2))
		colnames(EVNTS) <- c("LOC", "TYP")
		EVNTS$LOC <- c(entr, exit, QTR)
		EVNTS$TYP <- c(rep("ENTER", length(entr)), rep("EXIT", length(exit)), rep("QRTR", length(QTR)))
		EVNTS <- EVNTS[order(EVNTS$LOC), ]
	        for(i in 2:(nrow(EVNTS)-1)){
			if(EVNTS$TYP[i] == "ENTER"){
				tm1.gm[as.numeric(row.names(tm1.gm)) == EVNTS$LOC[i], j] <- 1
				tm1.gm[as.numeric(row.names(tm1.gm)) < EVNTS$LOC[i] & as.numeric(row.names(tm1.gm)) > EVNTS$LOC[i-1], j] <- 0
				tm1.gm[as.numeric(row.names(tm1.gm)) > EVNTS$LOC[i] & as.numeric(row.names(tm1.gm)) < EVNTS$LOC[i+1], j] <- 1				
			}else if(EVNTS$TYP[i] == "EXIT"){
				tm1.gm[as.numeric(row.names(tm1.gm)) == EVNTS$LOC[i], j] <- 1				
				tm1.gm[as.numeric(row.names(tm1.gm)) < EVNTS$LOC[i] & as.numeric(row.names(tm1.gm)) > EVNTS$LOC[i-1], j] <- 1				
				tm1.gm[as.numeric(row.names(tm1.gm)) > EVNTS$LOC[i] & as.numeric(row.names(tm1.gm)) < EVNTS$LOC[i+1], j] <- 0				
			}else{
				if(EVNTS$TYP[i-1] == "QRTR"){
					tm1.gm[as.numeric(row.names(tm1.gm)) < EVNTS$LOC[i] & as.numeric(row.names(tm1.gm)) > EVNTS$LOC[i-1], j] <- 
						ifelse(length(mentions[mentions < EVNTS$LOC[i] & mentions > EVNTS$LOC[i-1]]) > 0, 1, 0)
				}else{
				}
				if(EVNTS$TYP[i+1] == "QRTR"){
					tm1.gm[as.numeric(row.names(tm1.gm)) > EVNTS$LOC[i] & as.numeric(row.names(tm1.gm)) < EVNTS$LOC[i+1], j] <- 
						ifelse(length(mentions[mentions > EVNTS$LOC[i] & mentions < EVNTS$LOC[i+1]]) > 0, 1, 0)
				}else{
				}				
			}
		}
		if(length(grep(paste(NAME, "ejected", sep=""), tm1.gm[,1])) > 0){
			tm1.gm[as.numeric(row.names(tm1.gm)) > grep(paste(NAME, "ejected", sep=""), tm1.gm[,1]), j] <- 0
		}else{
		}		
	}
tm1.gm <- subset(tm1.gm, !(as.numeric(row.names(tm1.gm)) %in% QTR))
row.names(tm1.gm) <- 1:nrow(tm1.gm)

# swap in IDs for 1s
for(j in 4:ncol(tm1.gm)){
	tm1.gm[,j] <- ifelse(tm1.gm[,j] == 1, as.character(subset(players, HOME == 0)$ID[j-3]), NA)
}

#  Prepare final team file and grab relevant statistics
TM1.gm <- data.frame(tm1.gm[1:3], "P1" = NA, "P2" = NA, "P3" = NA, "P4" = NA, "P5" = NA)
for(i in 1:nrow(tm1.gm)){
	MISSING <- length(tm1.gm[i, 4:ncol(tm1.gm)][!is.na(tm1.gm[i, 4:ncol(tm1.gm)])])
	TM1.gm[i, 4:ncol(TM1.gm)] <- c(tm1.gm[i, 4:ncol(tm1.gm)][!is.na(tm1.gm[i, 4:ncol(tm1.gm)])], rep(NA, (5-MISSING)))	
}

for(i in 1:nrow(TM1.gm)){
	TM1.gm$X2P[i] <-  ifelse(length(grep("(?=.*makes)(?!.*3-pt)(?!.*free throw)", TM1.gm[i,1], ignore.case=T, perl=T)) > 0, 1, 0)
	TM1.gm$X2PA[i] <- ifelse(length(grep("(?=.*makes|misses)(?!.*3-pt)(?!.*free throw)", TM1.gm[i,1], ignore.case=T, perl=T)) > 0, 1, 0)
	TM1.gm$X3P[i] <- ifelse(length(grep("(?=.*makes)(?=.*3-pt)", TM1.gm[i,1], ignore.case=T, perl=T)) > 0, 1, 0)
	TM1.gm$X3PA[i] <- ifelse(length(grep("3-pt", TM1.gm[i,1], ignore.case=T, perl=T)) > 0, 1, 0)
	TM1.gm$FT[i] <-  ifelse(length(grep("(?=.*makes)(?=.*free throw)", TM1.gm[i,1], ignore.case=T, perl=T)) > 0, 1, 0)
	TM1.gm$FTA[i] <- ifelse(length(grep("free throw", TM1.gm[i,1], ignore.case=T)) > 0, 1, 0)	
	TM1.gm$DRB[i] <- ifelse(length(grep("(?=.*defensive)(?=.*rebound)", TM1.gm[i,1], ignore.case=T, perl=T)) > 0, 1, 0)
	TM1.gm$ORB[i] <- ifelse(length(grep("(?=.*offensive)(?=.*rebound)", TM1.gm[i,1], ignore.case=T, perl=T)) > 0
				       & length(grep("1 of 2|1 of 3|2 of 3", TM1.gm[i-1,1], ignore.case=T, perl=T)) == 0, 1, 0)
	TM1.gm$TOV[i] <- ifelse(length(grep("turnover", TM1.gm[i,1], ignore.case=T)) > 0, 1, 0)		
	TM1.gm$POSS[i] <- (ifelse(length(grep("free throw 1 of 1|free throw 2 of 2|free throw 3 of 3", TM1.gm[i,1], ignore.case=T)) > 0, 1, 0) + TM1.gm$X2PA[i] + TM1.gm$X3PA[i] + TM1.gm$TOV[i] - TM1.gm$ORB[i])
}

for(i in 1:nrow(TM1.gm)){
	TM1.gm$AWAY.lnp[i] <- paste(c(TM1.gm$P1[i], TM1.gm$P2[i], TM1.gm$P3[i], TM1.gm$P4[i], TM1.gm$P5[i]), collapse = " + ")
}

#############  BUILD FOR HOME TEAM

	#build and clean a dataframe to plug in players from team 1
	player.mtx <- matrix(nrow = nrow(pbp), ncol = nrow(subset(players, HOME == 1)))
	tm2.gm <- data.frame(pbp, player.mtx)
	colnames(tm2.gm) <-c(colnames(pbp), as.character(subset(players, HOME == 1)$Name.shrt))

	tm2.gm[,1] <- gsub("$", " ", tm2.gm[,1])
	tm2.gm[,2] <- gsub("$", " ", tm2.gm[,2])
	

	for(j in 4:ncol(tm2.gm)){
		NAME <- paste(colnames(tm2.gm)[j], "")
		if(length(unique(players$Name.shrt)) == length(players$Name.shrt)){
			mentions <- c(grep(NAME, tm2.gm[, 1]), grep(NAME, tm2.gm[, 2]))
		}else{
			mentions <- c(grep(NAME, tm2.gm[, 2]))
			overlaps <- sort(c(grep(paste("foul|block|steal by|three seconds by", NAME), tm2.gm[, 2]), grep(paste(NAME, "gains possession", sep=""), tm2.gm[, 2]), grep("Jump ball", tm2.gm[, 2])))			
			if(length(overlaps) > 0){
				sort(mentions <- mentions[!(mentions %in% overlaps)])
			}				
		}
			offcourt <- c(grep("Technical|ejected", tm2.gm[, 1], ignore.case=T), grep("Technical|ejected", tm2.gm[, 2], ignore.case=T))
			if(length(offcourt) > 0){
				sort(mentions <- mentions[!(mentions %in% offcourt)])
			}		
		entr <- grep(paste("enter", NAME), tm2.gm[,2])
		exit <- grep(paste("the game for", NAME), tm2.gm[,2])
		EVNTS <- as.data.frame(matrix(nrow = length(c(entr, exit, QTR)), ncol = 2))
		colnames(EVNTS) <- c("LOC", "TYP")
		EVNTS$LOC <- c(entr, exit, QTR)
		EVNTS$TYP <- c(rep("ENTER", length(entr)), rep("EXIT", length(exit)), rep("QRTR", length(QTR)))
		EVNTS <- EVNTS[order(EVNTS$LOC), ]
	        for(i in 2:(nrow(EVNTS)-1)){
			if(EVNTS$TYP[i] == "ENTER"){
				tm2.gm[as.numeric(row.names(tm2.gm)) == EVNTS$LOC[i], j] <- 1
				tm2.gm[as.numeric(row.names(tm2.gm)) < EVNTS$LOC[i] & as.numeric(row.names(tm2.gm)) > EVNTS$LOC[i-1], j] <- 0
				tm2.gm[as.numeric(row.names(tm2.gm)) > EVNTS$LOC[i] & as.numeric(row.names(tm2.gm)) < EVNTS$LOC[i+1], j] <- 1				
			}else if(EVNTS$TYP[i] == "EXIT"){
				tm2.gm[as.numeric(row.names(tm2.gm)) == EVNTS$LOC[i], j] <- 1				
				tm2.gm[as.numeric(row.names(tm2.gm)) < EVNTS$LOC[i] & as.numeric(row.names(tm2.gm)) > EVNTS$LOC[i-1], j] <- 1				
				tm2.gm[as.numeric(row.names(tm2.gm)) > EVNTS$LOC[i] & as.numeric(row.names(tm2.gm)) < EVNTS$LOC[i+1], j] <- 0				
			}else{
				if(EVNTS$TYP[i-1] == "QRTR"){
					tm2.gm[as.numeric(row.names(tm2.gm)) < EVNTS$LOC[i] & as.numeric(row.names(tm2.gm)) > EVNTS$LOC[i-1], j] <- 
						ifelse(length(mentions[mentions < EVNTS$LOC[i] & mentions > EVNTS$LOC[i-1]]) > 0, 1, 0)
				}else{
				}
				if(EVNTS$TYP[i+1] == "QRTR"){
					tm2.gm[as.numeric(row.names(tm2.gm)) > EVNTS$LOC[i] & as.numeric(row.names(tm2.gm)) < EVNTS$LOC[i+1], j] <- 
						ifelse(length(mentions[mentions > EVNTS$LOC[i] & mentions < EVNTS$LOC[i+1]]) > 0, 1, 0)
				}else{
				}				
			}
		}
		if(length(grep(paste(NAME, "ejected", sep = ""), tm2.gm[,2])) > 0){
			tm2.gm[as.numeric(row.names(tm2.gm)) > grep(paste(NAME, "ejected", sep = ""), tm2.gm[,2]), j] <- 0
		}else{
		}
	}

tm2.gm <- subset(tm2.gm, !(as.numeric(row.names(tm2.gm)) %in% QTR))


# swap in full names for 1s
for(j in 4:ncol(tm2.gm)){
	tm2.gm[,j] <- ifelse(tm2.gm[,j] == 1, as.character(subset(players, HOME == 1)$ID[j-3]), NA)
}


#  Prepare final team file and grab relevant statistics
TM2.gm <- data.frame(tm2.gm[1:3], "P1" = NA, "P2" = NA, "P3" = NA, "P4" = NA, "P5" = NA)
for(i in 1:nrow(tm2.gm)){
	MISSING <- length(tm2.gm[i, 4:ncol(tm2.gm)][!is.na(tm2.gm[i, 4:ncol(tm2.gm)])])
	TM2.gm[i, 4:ncol(TM2.gm)] <- c(tm2.gm[i, 4:ncol(tm2.gm)][!is.na(tm2.gm[i, 4:ncol(tm2.gm)])], rep(NA, (5-MISSING)))
}

###

for(i in 1:nrow(TM2.gm)){
	TM2.gm$X2P[i] <-  ifelse(length(grep("(?=.*makes)(?!.*3-pt)(?!.*free throw)", TM2.gm[i,2], ignore.case=T, perl=T)) > 0, 1, 0)
	TM2.gm$X2PA[i] <- ifelse(length(grep("(?=.*makes|misses)(?!.*3-pt)(?!.*free throw)", TM2.gm[i,2], ignore.case=T, perl=T)) > 0, 1, 0)
	TM2.gm$X3P[i] <- ifelse(length(grep("(?=.*makes)(?=.*3-pt)", TM2.gm[i,2], ignore.case=T, perl=T)) > 0, 1, 0)
	TM2.gm$X3PA[i] <- ifelse(length(grep("3-pt", TM2.gm[i,2], ignore.case=T, perl=T)) > 0, 1, 0)
	TM2.gm$FT[i] <-  ifelse(length(grep("(?=.*makes)(?=.*free throw)", TM2.gm[i,2], ignore.case=T, perl=T)) > 0, 1, 0)
	TM2.gm$FTA[i] <- ifelse(length(grep("free throw", TM2.gm[i,2], ignore.case=T)) > 0, 1, 0)	
	TM2.gm$DRB[i] <- ifelse(length(grep("(?=.*defensive)(?=.*rebound)", TM2.gm[i,2], ignore.case=T, perl=T)) > 0, 1, 0)
	TM2.gm$ORB[i] <- ifelse(length(grep("(?=.*offensive)(?=.*rebound)", TM2.gm[i,2], ignore.case=T, perl=T)) > 0
				       & length(grep("1 of 2|1 of 3|2 of 3", TM2.gm[i-1,2], ignore.case=T, perl=T)) == 0, 1, 0)
	TM2.gm$TOV[i] <- ifelse(length(grep("turnover", TM2.gm[i,2], ignore.case=T)) > 0, 1, 0)		
	TM2.gm$POSS[i] <- (ifelse(length(grep("free throw 1 of 1|free throw 2 of 2|free throw 3 of 3", TM2.gm[i,2], ignore.case=T)) > 0, 1, 0) + TM2.gm$X2PA[i] + TM2.gm$X3PA[i] + TM2.gm$TOV[i] - TM2.gm$ORB[i])	
}

###
for(i in 1:nrow(TM2.gm)){
	TM2.gm$HOME.lnp[i] <- paste(c(TM2.gm$P1[i], TM2.gm$P2[i], TM2.gm$P3[i], TM2.gm$P4[i], TM2.gm$P5[i]), collapse = " + ")
}

#############  PULL IT ALL TOGETHER AND COLLAPSE STINTS ####################
AWAY <- data.frame(TM1.gm$AWAY.lnp, TM2.gm$HOME.lnp, TM1.gm[,9:(ncol(TM1.gm)-1)], TM2.gm[,9:(ncol(TM2.gm)-1)])
	colnames(AWAY)[1:2] <- c("LINEUP", "CP_LINEUP")
	AWAY <- aggregate(. ~ LINEUP + CP_LINEUP, data = AWAY, sum)
	AWAY <- data.frame("YEAR"=as.numeric(GM.hld[1]), "MONTH"=as.numeric(GM.hld[2]), "DAY"=as.numeric(GM.hld[3]), 
				  "Tm"=teams[1], "Opp"=teams[2], "HOME" = 0, AWAY)

HOME <- data.frame(TM2.gm$HOME.lnp, TM1.gm$AWAY.lnp, TM2.gm[,9:(ncol(TM2.gm)-1)], TM1.gm[,9:(ncol(TM1.gm)-1)])
	colnames(HOME)[1:2] <- c("LINEUP", "CP_LINEUP")
	HOME <- aggregate(.~ LINEUP + CP_LINEUP, data = HOME, sum)
	HOME <- data.frame("YEAR"=as.numeric(GM.hld[1]), "MONTH"=as.numeric(GM.hld[2]), "DAY"=as.numeric(GM.hld[3]), 
				  "Tm"=teams[2], "Opp"=teams[1], "HOME" = 1, HOME)

GAME <- rbind(AWAY, HOME)

# Weights...
	#O
GAME$FGA_O <- GAME$X2PA + GAME$X3PA
GAME$POSS.to_O <- GAME$POSS + GAME$ORB
GAME$RB.chance_O <- GAME$ORB + GAME$DRB.1
	#D
GAME$FGA_D <- GAME$X2PA.1 + GAME$X3PA.1
GAME$POSS.to_D <- GAME$POSS.1 + GAME$ORB.1
GAME$RB.chance_D <- GAME$ORB.1 + GAME$DRB

# DVs
	#O
GAME$EFG_O <- ifelse(GAME$FGA_O > 0, (GAME$X2P + GAME$X3P*1.5)/GAME$FGA_O, 0) # Weight by FGA in final analysis.
GAME$TOV_O <- ifelse(GAME$POSS.to_O > 0, GAME$TOV/GAME$POSS.to_O, 0) # Weight by TO-adjusted Poss in final analysis.
GAME$REB_O <- ifelse(GAME$RB.chance_O > 0, GAME$ORB/GAME$RB.chance_O, 0) # (OUTPUT$ORB + OUTPUT$DRB.1) as weight
GAME$FT_O <- ifelse(GAME$FGA_O > 0, GAME$FT/GAME$FGA_O, 0) # Weight by FGA in final analysis. 
	#D
GAME$EFG_D <- ifelse(GAME$FGA_D > 0, (GAME$X2P.1 + GAME$X3P.1*1.5)/GAME$FGA_D, 0) # Weight by FGA in final analysis.
GAME$TOV_D <- ifelse(GAME$POSS.to_D > 0, GAME$TOV.1/GAME$POSS.to_D, 0) # Weight by TO-adjusted Poss in final analysis.
GAME$REB_D <- ifelse(GAME$RB.chance_D > 0, GAME$DRB/GAME$RB.chance_D, 0) # (OUTPUT$ORB + OUTPUT$DRB.1) as weight
GAME$FT_D <- ifelse(GAME$FGA_D > 0, GAME$FT.1/GAME$FGA_D, 0) # Weight by FGA in final analysis. 

#  Null out the basic stats...
GAME <- GAME[-c(9:17, 19:27)]
GAME <- subset(GAME, POSS > 0 | POSS.1 >0)

LINEUP.DMP <- rbind(LINEUP.DMP, GAME)
PLAYER.DMP <- subset(rbind(PLAYER.DMP, players), !duplicated(ID))
	}

	}
}



# restart on  2934
#   13



##   Problem in 351 5.
##   Problem in 518 12.
##   Problem in 2223 11.





write.csv(LINEUP.DMP, "LINEUP_DMP.csv", row.names = F)
write.csv(PLAYER.DMP, "PLAYER_DMP.csv", row.names = F)



# PREP FOR RAPM ANALYSIS

LINEUP.DMP <- LINEUP.DMP[!duplicated(LINEUP.DMP), ]
LINEUP.DMP$Season <- ifelse(LINEUP.DMP$MONTH > 7, LINEUP.DMP$YEAR + 1, LINEUP.DMP$YEAR)

PLAYER.DMP <- na.omit(PLAYER.DMP)
dummies <- as.character(PLAYER.DMP$ID)

LUP_RPM <- LINEUP.DMP
LUP_RPM[dummies] <- NA


write.csv(LUP_RPM, "LUP_RPM.csv", row.names = F)


####  PLAYERS ADD IN STATS!!!! 

plr <- PLAYER.DMP

TOT <- data.frame("ID"=NA, "Season"=NA, "Age"=NA, "Tm"=NA, "Lg"=NA, "Pos"=NA, "G"=NA, "GS"=NA, "MP"=NA, "FG"=NA, "FGA"=NA, "FG%"=NA, "3P"=NA, "3PA"=NA, "3P%"=NA, "2P"=NA,"2PA"=NA, "2P%"=NA, "eFG%"=NA, "FT"=NA, "FTA"=NA, "FT%"=NA, "ORB"=NA, "DRB"=NA, "TRB"=NA, "AST"=NA, "STL"=NA, "BLK"=NA, "TOV"=NA, "PF"=NA, "PTS"=NA)
ADV <- data.frame("ID"=NA, "Season"=NA, "Tm"=NA, "ORB%"=NA, "DRB%"=NA, "TRB%"=NA, "AST%"=NA, "STL%"=NA, "BLK%"=NA, "TOV%"=NA, "USG%"=NA)
SHT <- data.frame("ID"=NA, "Season"=NA, "Tm"=NA, "Dist."=NA, "per2P"=NA, "per0-3"=NA, "per3-10"=NA, "per10-16"=NA, "per16-23"=NA, "per3P"=NA, "eff2P"=NA, "eff0-3"=NA, "eff3-10"=NA, "eff10-16"=NA, "eff16-23"=NA, "eff3P"=NA, "Asd%2"=NA, "perDNK"=NA, "DNK"=NA, "Asd%2"=NA, "perCRNR"=NA, "CRNR%"=NA, "HEAVEa"=NA, "HEAVEm"=NA)
	
for(i in 1:nrow(plr)){
	plr.url <- paste0("http://www.basketball-reference.com/players/", substring(plr[i,3], 1, 1), "/", substring(plr[i,3], 2), ".html")
		hld <- GET(plr.url)
		hld <- readHTMLTable(rawToChar(hld$content), stringsAsFactors = F)
		if(length(hld) > 1){
			tot <- as.data.frame(hld[["totals"]])
				tot <- data.frame("ID"=plr[i,3], tot)
			adv <- as.data.frame(hld[["advanced"]])[c(1,3,12:19)]
				adv <- data.frame("ID"=plr[i,3], adv)	
			sht <- as.data.frame(hld[["shooting"]])[-c(2, 4:8)]
				sht <- data.frame("ID"=plr[i,3], sht)			
				colnames(sht) <- colnames(SHT)
			TOT <- rbind(TOT, tot) 
			ADV <- rbind(ADV, adv) 
			SHT <- rbind(SHT, sht)
		}else{
		}
}     
STATS <- merge(TOT, ADV, by = c("ID", "Season", "Tm"), all.x = T)
STATS <- merge(STATS, SHT, by = c("ID", "Season", "Tm"), all.x = T)
STATS$Season <- as.numeric(substr(STATS$Season,start=1,stop=4)) + 1
tst <- merge(STATS, plr, by = "ID", all.x = T)
tst <- subset(tst, Tm != "TOT" & !is.na(Lg))

write.csv(tst, "PLR_RPM.csv", row.names = FALSE)

#1176

