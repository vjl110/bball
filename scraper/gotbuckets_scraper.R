setwd("~/GitHub/bball")

library(XML)
library(RCurl)
library(httr)

###
##### NBA COMP DUMP STATISTICS
###

off.dmp <- data.frame("Name" = NA, "OEFF" = NA, "efg.ffo" = NA, "tov.ffo" = NA, "reb.ffo" = NA, "ft.ffo" = NA, "MP" = NA, "Pos" = NA, "Tm" = NA, "Season" = NA) 
def.dmp <- data.frame("Name" = NA, "DEFF" = NA, "efg.ffd" = NA, "tov.ffd" = NA, "reb.ffd" = NA, "ft.ffd" = NA, "MP" = NA, "Pos" = NA, "Tm" = NA, "Season" = NA) 
for(i in 2005:2015){
	url.o <- paste(c("http://www.gotbuckets.com/", i, "-ffapm-offense/"), collapse="")
	url.d <- paste(c("http://www.gotbuckets.com/", i, "-ffapm-defense/"), collapse="")		    
	off <- GET(url.o)
		off <- readHTMLTable(rawToChar(off$content), stringsAsFactors = F)
			off <- as.data.frame(off)
			off <- data.frame(off, "Season" = i)	
			colnames(off) <- c("Name", "OEFF", "efg.ffo", "tov.ffo", "reb.ffo", "ft.ffo", "MP", "Pos", "Tm", "Season") 
			off.dmp <- rbind(off.dmp, off)
	def <- GET(url.d)
		def <- readHTMLTable(rawToChar(def$content), stringsAsFactors = F)
			def <- as.data.frame(def)
			def <- data.frame(def, "Season" = i)
			colnames(def) <- c("Name", "DEFF", "efg.ffd", "tov.ffd", "reb.ffd", "ft.ffd", "MP", "Pos", "Tm", "Season") 
			def$Season == i			
			def.dmp <- rbind(def.dmp, def)
}
	
write.csv(off.dmp, "data/gbo.csv")
write.csv(def.dmp, "data/gbd.csv")


