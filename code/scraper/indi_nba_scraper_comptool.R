setwd("~/GitHub/bball")

library(XML)
library(RCurl)
library(httr)

###
##### NBA COMP DUMP STATISTICS
###


dmp <- data.frame("Name"=NA, "Season"=NA, "Tm"=NA, "Lg"=NA, "Pos"=NA, "G"=NA, "GS"=NA, "MP"=NA,	"X2P"=NA, "X2PA"=NA, "X3P"=NA, "X3PA"=NA, "FT"=NA, "FTA"=NA, "ORB"=NA, "DRB"=NA, "TRB"=NA, "AST"=NA, "STL"=NA, "BLK"=NA, "TOV"=NA, "PF"=NA, "PTS"=NA, "Age"=NA)

i <- 0
kill <- c(0, 0, 0)
while(sum(kill) < 1){
	url <- paste(c("http://www.basketball-reference.com/play-index/psl_finder.cgi?request=1&match=single&type=totals&per_minute_base=36&per_poss_base=100&lg_id=NBA&is_playoffs=N&year_min=2015&year_max=2015&franch_id=&season_start=1&season_end=-1&age_min=0&age_max=99&shoot_hand=&height_min=0&height_max=99&birth_country_is=Y&birth_country=&birth_state=&college_id=&is_active=&debut_yr_aba_start=&debut_yr_aba_end=&debut_yr_nba_start=&debut_yr_nba_end=&is_hof=&is_as=&as_comp=gt&as_val=&award=&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&qual=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5stat=&c5comp=gt&c6mult=1.0&c6stat=&order_by=ws&order_by_asc=&offset=", 0 + i),sep="",collapse="")
	cmp <- GET(url)
		cmp <- readHTMLTable(rawToChar(cmp$content), stringsAsFactors = F)
			cmp <- as.data.frame(cmp$stats)
			if(length(cmp$Player) == 0){
				kill[1] <- 1
			}else{
				cmp <- data.frame("Name"=cmp[,2], "Season"=cmp[,3], "Tm"=cmp[,5], "Lg"=cmp[,6], "Pos"=NA, "G"=cmp[,7], "GS"=cmp[,8], "MP"=cmp[,9],	"X2P"=cmp[,12], "X2PA"=cmp[,13], "X3P"=cmp[,14], "X3PA"=cmp[,15], "FT"=cmp[,16], "FTA"=cmp[,17], "ORB"=cmp[,18], "DRB"=cmp[,19], "TRB"=cmp[,20], "AST"=cmp[,21], "STL"=cmp[,22], "BLK"=cmp[,23], "TOV"=cmp[,24], "PF"=cmp[,25], "PTS"=cmp[,26], "Age"=cmp[,4])	
				cmp <- subset(cmp, Name != "Player")
				dmp <- rbind(dmp, cmp)	
			}	
	i <- i + 100			
}
dmp <- dmp[-1,]
	dmp$Season <- as.character(dmp$Season)
	dmp$Season <- substr(dmp$Season, 1, 4)
	dmp$Season <- as.numeric(dmp$Season) + 1

write.csv(dmp, "data/draftmod/BBRtot2015.csv", row.names = FALSE)

##########
### ADV
##########

dmp <- data.frame("Name"=NA, "Season"=NA, "Age"=NA, "Tm"=NA, "Lg"=NA, "Pos"=NA, "G"=NA, "MP"=NA, "PER"=NA, "TS%"=NA, "eFG%"=NA, "ORB%"=NA, "DRB%"=NA, "TRB%"=NA, "AST%"=NA, "STL%"=NA, "BLK%"=NA, "TOV%"=NA, "USG%"=NA, "ORtg"=NA, "DRtg"=NA, "OWS"=NA, "DWS"=NA, "WS"=NA, "WS/48"=NA)

i <- 0
kill <- c(0, 0, 0)
while(sum(kill) < 1){
	url <- paste(c("http://www.basketball-reference.com/play-index/psl_finder.cgi?request=1&match=single&type=advanced&per_minute_base=36&per_poss_base=100&lg_id=NBA&is_playoffs=N&year_min=2015&year_max=2015&franch_id=&season_start=1&season_end=-1&age_min=0&age_max=99&shoot_hand=&height_min=0&height_max=99&birth_country_is=Y&birth_country=&birth_state=&college_id=&is_active=&debut_yr_aba_start=&debut_yr_aba_end=&debut_yr_nba_start=&debut_yr_nba_end=&is_hof=&is_as=&as_comp=gt&as_val=&award=&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&qual=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5stat=&c5comp=gt&c6mult=1.0&c6stat=&order_by=ws&order_by_asc=&offset=", 0 + i),sep="",collapse="")
	cmp <- GET(url)
			cmp <- readHTMLTable(rawToChar(cmp$content), stringsAsFactors = F)
			cmp <- as.data.frame(cmp$stats)
			if(length(cmp$Player) == 0){
				kill[1] <- 1
			}else{
				cmp <- data.frame("Name"=cmp[,2], "Season"=cmp[,3], "Age"=cmp[,4], "Tm"=cmp[,5], "Lg"=cmp[,6], "Pos"=NA, "G"=cmp[,7], "MP"=cmp[,9], "PER"=cmp[,10], "TS%"=cmp[,11], "eFG%"=cmp[,12], "ORB%"=cmp[,13], "DRB%"=cmp[,14], "TRB%"=cmp[,15], "AST%"=cmp[,16], "STL%"=cmp[,17], "BLK%"=cmp[,18], "TOV%"=cmp[,19], "USG%"=cmp[,20], "ORtg"=cmp[,21], "DRtg"=cmp[,22], "OWS"=cmp[,23], "DWS"=cmp[,24], "WS"=cmp[,25], "WS/48"=cmp[,26])	
				cmp <- subset(cmp, Name != "Player")
				dmp <- rbind(dmp, cmp)	
			}	
	i <- i + 100			
}
dmp <- dmp[-1,]
	dmp$Season <- as.character(dmp$Season)
	dmp$Season <- substr(dmp$Season, 1, 4)
	dmp$Season <- as.numeric(dmp$Season) + 1

write.csv(dmp, "data/draftmod/BBRadv2015.csv", row.names = FALSE)

