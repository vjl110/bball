setwd("C:/Users/Layne/Dropbox/bball")

library(XML)
library(RCurl)
library(httr)
library(stringr)


NBA.base <- data.frame("name"=NA, "bgn"=NA, "end"=NA, "pos"=NA, "hgt"=NA, "wgt"=NA, "college"=NA, "day"=NA, "mnth"=NA, "year"=NA, "bbr"=NA, "id"=NA) 
for(i in c(1:23, 25:26)){
	url <- paste0("http://www.basketball-reference.com/players/", letters[i], "/")
		tab <- GET(url)
		tab <- readHTMLTable(rawToChar(tab$content), stringsAsFactors = F)
		tab <- as.data.frame(tab)
			day <- str_split_fixed(tab$players.Birth.Date, fixed(" "), 3)[, 2]
			mnth <- str_split_fixed(tab$players.Birth.Date, fixed(" "), 3)[, 1]
			year <- str_split_fixed(tab$players.Birth.Date, fixed(" "), 3)[, 3]
			tab <- tab[c(1:6, 8)]
			colnames(tab) <- c("name", "bgn", "end", "pos", "hgt", "wgt", "college")
				tab$day <- gsub(",", "", day)
				tab$mnth <- mnth
				tab$year <- year	
			tab$hgt <- as.numeric(str_split_fixed(tab$hgt, fixed("-"), 2)[, 2]) + as.numeric(str_split_fixed(tab$hgt, fixed("-"), 2)[, 1])*12
			tab$name <- gsub("\\*", "", tab$name )	
		
		IDs <- GET(url)
		IDs <- content(IDs, as="text")
		IDs <- xpathSApply(htmlParse(IDs), "//a/@href")
		IDs <- unique(as.character(IDs[grep("players/", IDs)]))[-1]
		IDs <- gsub("players", "", IDs)
		IDs <- gsub("html", "", IDs)			
		IDs <- gsub("[^A-Za-z0-9]", "", IDs)
		IDs <- sub('^.', "", IDs)
		IDs <- IDs[IDs != ""]

		tab$bbr <- IDs
		tab$id <- paste0(tolower(gsub("[^A-Za-z0-9]", "", tab$name)), tab$day, tolower(substr(tab$mnth, 1, 3)), tab$year)
	NBA.base <- rbind(NBA.base, tab)
}
NBA.base <- NBA.base[-1,]


###
#####
##


IDs <- data.frame("bbr"=NBA.base$bbr, "id"=NBA.base$id)

TOT <- data.frame("Season"=NA,"Age"=NA,"Tm"=NA,"Lg"=NA,"Pos"=NA, "G"=NA,"GS"=NA,"MP"=NA,"X3P"=NA, "X3PA"=NA, "X2P"=NA, "X2PA"=NA,"FT"=NA, "FTA"=NA, "ORB"=NA, "DRB"=NA, "TRB"=NA, "AST"=NA, "STL"=NA,"BLK"=NA, "TOV"=NA, "PF"=NA, "PTS"=NA, "id"=NA)
ADV <- data.frame("Season"=NA, "Age"=NA, "Tm"=NA, "Lg"=NA, "Pos"=NA, "G"=NA, "MP"=NA, "PER"=NA,"ORB."=NA, "DRB."=NA, "TRB."=NA, "AST."=NA, "STL."=NA, "BLK."=NA,"TOV."=NA, "USG."=NA, "OWS"=NA, "DWS"=NA,"OBPM"=NA, "DBPM"=NA, "VORP"=NA, "id"=NA)
SHT <- data.frame("Season"=NA, "Age"=NA, "Tm"=NA, "Lg"=NA, "Pos"=NA, "G"=NA, "MP"=NA, "Dist."=NA, "rim"=NA, "cls"=NA, "mid"=NA, "lng"=NA, "tre"=NA,"rim."=NA, "cls."=NA, "mid."=NA, "lng."=NA, "tre."=NA, "asd2"=NA, "dnk"=NA, "dnk.tot"=NA, "asd3"=NA, "crnr"=NA, "crnr."=NA, "hv_at"=NA, "hv_md"=NA, "id"=NA) 
#PBP <- data.frame("Season"=NA, "Age"=NA, "Tm"=NA, "Lg"=NA, "Pos"=NA, "G"=NA, "MP"=NA, "PG."=NA, "SG."=NA, "SF."=NA, "PF."=NA, "C."=NA, "On"=NA, "OnOff"=NA, "OF"=NA, "BadPass"=NA, "LostBall"=NA, "Other"=NA, "PGA"=NA, "And1"=NA, "Drawn"=NA, "Blkd"=NA, "id"=NA)

TOT_PO <- TOT
ADV_PO <- ADV
SHT_PO <- SHT
#PBP_PO <- PBP

NCAA <- data.frame("Season"=NA, "Age"=NA, "College"=NA, "G"=NA, "MP"=NA, "FG"=NA, "FGA"=NA, "X3P"=NA, "X3PA"=NA, "FT"=NA, "FTA"=NA, "ORB"=NA, "TRB"=NA, "AST"=NA, "STL"=NA, "BLK"=NA, "TOV"=NA, "PF"=NA, "PTS"=NA, "id"=NA)
SAL <- data.frame("Season"=NA, "Team"=NA, "Lg"=NA, "Salary"=NA, "id"=NA)

for(i in 1:nrow(IDs)){
	url <- paste0("http://www.basketball-reference.com/players/", substr(IDs$bbr[i], 1, 1), "/", IDs$bbr[i], ".html")
		stat <- GET(url)
		stat <- readHTMLTable(rawToChar(stat$content), stringsAsFactors = F)	

		tot <- as.data.frame(stat$totals)[-c(9:11, 14, 17:18, 21)]
		colnames(tot)[9:12] <- c("X3P", "X3PA", "X2P", "X2PA")
		tot$id <- IDs$id[i]
			TOT <- rbind(TOT, tot)	
		adv <- as.data.frame(stat$advanced)[-c(9:11, 20, 23:25, 28)]
		colnames(adv)[9:16] <- c("ORB.", "DRB.", "TRB.", "AST.", "STL.", "BLK.","TOV.", "USG.")
		adv$id <- IDs$id[i]
			ADV <- rbind(ADV, adv)
	if(length(stat$shooting) > 0){
		sht <- as.data.frame(stat$shooting)[-c(8, 10, 16)]
		colnames(sht) <- c("Season", "Age", "Tm", "Lg", "Pos", "G", "MP", "Dist.", "rim", "cls", "mid", "lng", "tre","rim.", "cls.", "mid.", "lng.", "tre.", "asd2", "dnk", "dnk.tot", "asd3", "crnr", "crnr.", "hv_at", "hv_md") 
		sht$id <- IDs$id[i]
			SHT <- rbind(SHT, sht)
#		pbp <- as.data.frame(stat$advanced_pbp)
#		colnames(pbp) <- c("Season", "Age", "Tm", "Lg", "Pos", "G", "MP", "PG.", "SG.", "SF.", "PF.", "C.", "On", "OnOff", "OF", "BadPass", "LostBall", "Other", "PGA", "And1", "Drawn", "Blkd")
#		pbp$id <- IDs$id[i]
#			PBP <- rbind(PBP, pbp)
	}else{
	}
	if(length(stat$playoffs_totals) > 0){
		tot_po <- as.data.frame(stat$playoffs_totals)[-c(9:11, 14, 17:18, 21)]
		colnames(tot_po)[9:12] <- c("X3P", "X3PA", "X2P", "X2PA")
		tot_po$id <- IDs$id[i]
			TOT_PO <- rbind(TOT_PO, tot_po)
		adv_po <- as.data.frame(stat$playoffs_advanced)[-c(9:11, 20, 23:25, 28)]
		colnames(adv_po)[9:16] <- c("ORB.", "DRB.", "TRB.", "AST.", "STL.", "BLK.","TOV.", "USG.")
		adv_po$id <- IDs$id[i]
			ADV_PO <- rbind(ADV_PO, adv_po)
	if(length(stat$playoffs_shooting) > 0){	
		sht_po <- as.data.frame(stat$playoffs_shooting)[-c(8, 10, 16)]
		colnames(sht_po) <- c("Season", "Age", "Tm", "Lg", "Pos", "G", "MP", "Dist.", "rim", "cls", "mid", "lng", "tre","rim.", "cls.", "mid.", "lng.", "tre.", "asd2", "dnk", "dnk.tot", "asd3", "crnr", "crnr.", "hv_at", "hv_md")
		sht_po$id <- IDs$id[i]
			SHT_PO <- rbind(SHT_PO, sht_po)
#		pbp_po <- as.data.frame(stat$playoffs_advanced_pbp)
#		colnames(pbp_po) <- c("Season", "Age", "Tm", "Lg", "Pos", "G", "MP", "PG.", "SG.", "SF.", "PF.", "C.", "On", "OnOff", "OF", "BadPass", "LostBall", "Other", "PGA", "And1", "Drawn", "Blkd")
#		pbp_po$id <- IDs$id[i]
#			PBP_PO <- rbind(PBP_PO, pbp_po)
	}else{
	}
	}else{
	}
	if(length(stat$college) > 0){
		ncaa <- as.data.frame(stat$college)[1:19]
		colnames(ncaa)[8:9] <- c("X3P", "X3PA")
		ncaa$id <- IDs$id[i]
			NCAA <- rbind(NCAA, ncaa)
	}else{
	}
	if(length(stat$salaries) > 0){
		sal <- as.data.frame(stat$salaries)
		sal$id <- IDs$id[i]
			SAL <- rbind(SAL, sal)
	}else{
	}
}


write.csv(TOT, "data/TOT_pull.csv", row.names = F)
write.csv(ADV, "data/ADV_pull.csv", row.names = F)
write.csv(SHT, "data/SHT_pull.csv", row.names = F)

write.csv(TOT_PO, "data/TOTPO_pull.csv", row.names = F)
write.csv(ADV_PO, "data/ADVPO_pull.csv", row.names = F)
write.csv(SHT_PO, "data/SHTPO_pull.csv", row.names = F)

write.csv(SAL, "data/SAL_pull.csv", row.names = F)
write.csv(NCAA, "data/NCAA_pull.csv", row.names = F)


plr <- na.omit(TOT[c(1, 24)])
	plr$Season <- as.numeric(substr(plr$Season, 1, 4)) + 1
	plr <- merge(plr, NBA.base[c(1, 11:12)], by = "id", all.x = T)

write.csv(plr, "data/plr.csv", row.names = F)




library(sqldf)
db <- dbConnect(SQLite(), dbname="NBA.sqlite")
	dbWriteTable(conn = db, name = "base", value = NBA.base, row.names = FALSE)
	dbWriteTable(conn = db, name = "total", value = TOT, row.names = FALSE)
	dbWriteTable(conn = db, name = "advanced", value = ADV, row.names = FALSE)
	dbWriteTable(conn = db, name = "shooting", value = SHT, row.names = FALSE)
	dbWriteTable(conn = db, name = "pbp", value = PBP, row.names = FALSE)
	dbWriteTable(conn = db, name = "total_po", value = TOT_PO, row.names = FALSE)
	dbWriteTable(conn = db, name = "advanced_po", value = ADV_PO, row.names = FALSE)
	dbWriteTable(conn = db, name = "shooting_po", value = SHT_PO, row.names = FALSE)
	dbWriteTable(conn = db, name = "pbp_po", value = PBP_PO, row.names = FALSE)
dbDisconnect(db)


