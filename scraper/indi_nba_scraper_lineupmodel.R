setwd("~/GitHub/bball/scraper")


library(XML)
library(RCurl)
library(httr)


###
######  SHOOTING DATA ####
###

shot.type <- c("", "JUMP_SHOT", "DUNK", "HOOK_SHOT", "LAY-UP", "TIP_SHOT") ### Layup not in 2001 data
shot.lst <- list()
sea.lst <- list()
for(y in 2002:2015){
	for(t in 1:length(shot.type)){

	theurl <- paste(c("http://www.basketball-reference.com/play-index/plus/shot_finder.cgi?request=1&match=single&player_id=&year_id=", y, "&is_playoffs=&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&shot_pts=&is_make=&shot_type=", shot.type[t], "&shot_distance_min=&shot_distance_max=&q1=Y&q2=Y&q3=Y&q4=Y&q5=Y&time_remain_minutes=12&time_remain_seconds=0&time_remain_comp=le&margin_min=&margin_max=&is_tying=&is_go_ahead=&c1stat=&c1comp=ge&c1val=&c2stat=&c2comp=ge&c2val=&c3stat=&c3comp=ge&c3val=&order_by=fga&order_by_asc=&offset=0"),sep="",collapse="")
	tab <- GET(theurl)
		tab <- readHTMLTable(rawToChar(tab$content), stringsAsFactors = F)
			tab <- as.data.frame(tab)
			tab <- tab[-c(1:5)]
			tab <- tab[-c(2,4, 7:8, 11:13, 15)]
			colnames(tab) <- c("Name", "Tm", paste(shot.type[t]), paste(c(shot.type[t], ".a"), collapse=""), paste(c(shot.type[t], ".3p"), collapse=""),  paste(c(shot.type[t], ".3pa"), collapse=""), paste(c(shot.type[t], ".asd"), collapse=""))		

	base <- subset(tab, Name != "Player" & !is.na(Name))
	i <- 0
	kill <- 0
	while(kill == 0){
		i <- i + 100
		theurl <- paste(c("http://www.basketball-reference.com/play-index/plus/shot_finder.cgi?request=1&match=single&player_id=&year_id=", y, "&is_playoffs=&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&shot_pts=&is_make=&shot_type=", shot.type[t], "&shot_distance_min=&shot_distance_max=&q1=Y&q2=Y&q3=Y&q4=Y&q5=Y&time_remain_minutes=12&time_remain_seconds=0&time_remain_comp=le&margin_min=&margin_max=&is_tying=&is_go_ahead=&c1stat=&c1comp=ge&c1val=&c2stat=&c2comp=ge&c2val=&c3stat=&c3comp=ge&c3val=&order_by=fga&order_by_asc=&offset=", 0 + i),sep="",collapse="")
		tab <- GET(theurl)
			tab <- readHTMLTable(rawToChar(tab$content), stringsAsFactors = F)
			tab <- as.data.frame(tab)
			tab <- tab[-c(1:5)]
			tab <- tab[-c(2,4, 7:8, 11:13, 15)]
		if(length(tab$stats.Player) == 0){
			kill <- 1
		}else{
			colnames(tab) <- c("Name", "Tm", paste(shot.type[t]), paste(c(shot.type[t], ".a"), collapse=""), paste(c(shot.type[t], ".3p"), collapse=""),  paste(c(shot.type[t], ".3pa"), collapse=""), paste(c(shot.type[t], ".asd"), collapse=""))		
			tab <- subset(tab, Name != "Player" & !is.na(Name))
			base <- rbind(base, tab)
		}
	}
	shot.lst[[t]] <- base
	}
	fnl <- merge(shot.lst[[1]], shot.lst[[2]], by = c("Name", "Tm"), all.x = T) 	
	fnl <- merge(fnl, shot.lst[[3]], by = c("Name", "Tm"), all.x = T) 	
	fnl <- merge(fnl, shot.lst[[4]], by = c("Name", "Tm"), all.x = T) 	
	fnl <- merge(fnl, shot.lst[[5]], by = c("Name", "Tm"), all.x = T) 	
	fnl <- merge(fnl, shot.lst[[6]], by = c("Name", "Tm"), all.x = T)
	fnl$Season <- y

sea.lst[[y - 2001]] <- fnl	
}
shots <- rbind(sea.lst[[1]], sea.lst[[2]])
for(i in 3:length(sea.lst)){
	shots <- rbind(shots, sea.lst[[i]])
}
for(j in 3:ncol(shots)){
	shots[,j] <- as.numeric(as.character(shots[,j]))
}
shots[is.na(shots)] <- 0

#NEED TO FIX TIP ATTEMPTS ASAP!!!!
shtz <- data.frame("Name" = shots$Name, "Season" = shots$Season, "Tm" = shots$Tm, "JMP" = (shots[,8] - shots[,10]), "JMPA" = (shots[,9] - shots[,11]), "HOOK" = (shots[,18] - shots[,20]), "HOOKA" = (shots[,19] - shots[,21]), "LAY" = shots[,23], "LAYA" = shots[,24], "DNK" = shots[,13], "DNKA" = shots[,14], "TIP" = shots[,28], "TIPA" = shots[,29], "ASD" = ifelse(shots[,7] > 0, shots[,7]/(shots[,3] + shots[,5]), 0))

###
##### INDIVIDUAL STATISTICS
###

pm.dmp <- data.frame("Name"=NA, "Tm"=NA, "Season"=NA, "G"=NA, "MP"=NA, "Poss_O"=NA, "Poss_D"=NA, "Pace"=NA)
bas.dmp <- data.frame("Name"=NA, "Season"=NA, "Age"=NA, "Tm"=NA, "X2P"=NA, "X2PA"=NA, "X3P"=NA, "X3PA"=NA, "FT"=NA, "FTA"=NA, "ORB"=NA, "DRB"=NA, "AST"=NA, "STL"=NA, "BLK"=NA, "TOV"=NA, "PF"=NA, "PTS"=NA)
adv.dmp <- data.frame("Name"=NA, "Season"=NA, "Tm"=NA, "PER"=NA, "TS."=NA, "eFG."=NA, "ORB."=NA, "DRB."=NA, "TRB."=NA, "AST."=NA, "STL."=NA, "BLK."=NA, "TOV."=NA, "USG."=NA, "ORtg"=NA, "DRtg"=NA, "OWS"=NA, "DWS"=NA)

i <- 0
kill <- c(0, 0, 0)
while(sum(kill) < 3){
	basurl <- paste(c("http://www.basketball-reference.com/play-index/psl_finder.cgi?request=1&match=single&type=totals&per_minute_base=36&per_poss_base=100&lg_id=NBA&is_playoffs=N&year_min=2001&year_max=2015&franch_id=&season_start=1&season_end=-1&age_min=0&age_max=99&shoot_hand=&height_min=0&height_max=99&birth_country_is=Y&birth_country=&is_active=&is_hof=&is_as=&as_comp=gt&as_val=&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&qual=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5stat=&c5comp=gt&c6mult=1.0&c6stat=&order_by=mp&order_by_asc=&offset=", 0 + i),sep="",collapse="")
	pmurl <- paste(c("http://www.basketball-reference.com/play-index/plus/plus_minus_finder.cgi?request=1&match=single&player_id=&output=total&year_min=2001&year_max=2015&age_min=0&age_max=99&is_playoffs=N&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&c1stat=opp_fg&c1comp=ge&c1val=&c2stat=opp_orb&c2comp=ge&c2val=&c3stat=opp_tov&c3comp=ge&c3val=&c4stat=&c4comp=ge&c4val=&order_by=mp&order_by_asc=&offset=", 0 + i),sep="",collapse="")
	advurl <- paste(c("http://www.basketball-reference.com/play-index/psl_finder.cgi?request=1&match=single&type=advanced&per_minute_base=36&per_poss_base=100&lg_id=NBA&is_playoffs=N&year_min=2001&year_max=2015&franch_id=&season_start=1&season_end=-1&age_min=0&age_max=99&shoot_hand=&height_min=0&height_max=99&birth_country_is=Y&birth_country=&is_active=&is_hof=&is_as=&as_comp=gt&as_val=&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&qual=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5stat=&c5comp=gt&c6mult=1.0&c6stat=&order_by=mp&order_by_asc=&offset=", 0 + i),sep="",collapse="")
	pm <- GET(pmurl)
		pm <- readHTMLTable(rawToChar(pm$content), stringsAsFactors = F)
			pm <- as.data.frame(pm$stats)
			if(length(pm$Player) == 0){
				kill[1] <- 1
			}else{
				pm <- pm[c(2:9)]	
				colnames(pm)[c(1, 6:7)] <- c("Name", "Poss_O", "Poss_D")
				pm <- subset(pm, Name != "Poss")
				pm.dmp <- rbind(pm.dmp, pm)	
			}	
	bas <- GET(basurl)
		bas <- readHTMLTable(rawToChar(bas$content), stringsAsFactors = F)
			bas <- as.data.frame(bas$stats)
			if(length(bas$Player) == 0){
				kill[2] <- 1
			}else{	
				bas <- bas[c(2:5, 12:19, 21:26)]	
				colnames(bas)[c(1, 5:8)] <- c("Name", "X2P", "X2PA", "X3P", "X3PA")
				bas <- subset(bas, Name != "Player")
				bas.dmp <- rbind(bas.dmp, bas)	
			}	
	adv <- GET(advurl)	
		adv <- readHTMLTable(rawToChar(adv$content), stringsAsFactors = F)
			adv <- as.data.frame(adv$stats)
			if(length(adv$Player) == 0){
				kill[3] <- 1
			}else{		
				adv <- adv[c(2:3, 5, 10:24)]	
				colnames(adv) <- c("Name", "Season", "Tm", "PER", "TS.", "eFG.", "ORB.", "DRB.", "TRB.", "AST.", "STL.", "BLK.", "TOV.", "USG.", "ORtg", "DRtg", "OWS", "DWS")
				adv <- subset(adv, Name != "Player")	
				adv.dmp <- rbind(adv.dmp, adv)	
			
			}
	i <- i + 100			
}
indi <- merge(pm.dmp, bas.dmp, by = c("Name", "Season", "Tm"))
indi <- merge(indi, adv.dmp, by = c("Name", "Season", "Tm"))
	indi$Season <- as.character(indi$Season)
	indi$Season <- substr(indi$Season, 1, 4)
	indi$Season <- as.numeric(indi$Season) + 1

tst <- merge(indi, shtz, by = c("Name", "Season", "Tm"),)
write.csv(tst, "scraped_data/indi2lineup_data.csv", row.names = FALSE)


