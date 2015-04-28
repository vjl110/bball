rm(list=ls())
setwd("~/GitHub/bball/data")
#setwd("C:/Users/SCAN Project/Dropbox/Archive/BBall/Draft proj")


###
###### ARRANGE NBA DATA
###

### prep WS set
nba <- read.csv("BBRadv.csv", strip.white = T)
nba <- subset(nba, Tm != "TOT")
nba$Pos[is.na(nba$Pos)] <- 0
nba.sum <- nba[-c(3:7, 9:23, 25)]
nba.mean <- nba[-c(4:5, 7:25)]
nba.sum <- aggregate( . ~ Name + Season, data = nba.sum, sum)
nba.mean <- aggregate( . ~ Name + Season, data = nba.mean, mean)
nba <- merge(nba.mean, nba.sum, by=c('Name','Season'))
nba$WS[nba$Season==1999] <- nba$WS[nba$Season==1999]*(82/50)
nba$WS[nba$Season==2012] <- nba$WS[nba$Season==2012]*(82/66)

### add rapm data
rapm <- read.csv("rapm.csv", strip.white = T)
rapm <- rapm[-6]
nba <- merge(nba, rapm, by=c("Name", "Season"), all.x=T)
nba$RAPM <- round((nba$DIF + 3.5)*(nba$MP/3936)*3.15, digits= 2)
nba$RAPM[nba$Season==1999] <- nba$RAPM[nba$Season==1999]*(82/50)
nba$RAPM[nba$Season==2012] <- nba$RAPM[nba$Season==2012]*(82/66)
	# scale obs
mn.WS <- mean(nba$WS, na.rm=T)
sd.WS <- sd(nba$WS, na.rm=T)
nba$RAPM <- mn.WS + scale(nba$RAPM)*sd.WS
nba$CMB <- as.vector(ifelse(is.na(nba$RAPM), nba$WS, (nba$WS + nba$RAPM)/2))


#smooth away noise with means
nba <- nba[order(nba$Age),]

for(i in 1:nrow(nba)){
	nba$CMB[i] <- weighted.mean(subset(nba, Name == Name[i] & (Age == Age[i] | Age == (Age[i] - 1)))$CMB, subset(nba, Name == Name[i] & (Age == Age[i] | Age == (Age[i] - 1)))$MP, na.rm=T)
}

nba <- nba[-c(5:10)]

### find age adjustements ##

unq <- subset(nba, !duplicated(Name))$Name
nba$drop <- NA
for(i in 1:length(unq)){
	nba$drop[nba$Name == as.character(unq[i])] <- max(nba$Age[nba$Name == as.character(unq[i])], na.rm=T)
}

do.mat <- matrix(nrow = 26, ncol = 26)
for(i in 1:25){
	hld <- subset(nba, Season != 2014 & Age == (i + 18))
	for(j in (i + 1):26){
		hld2 <- subset(nba, Season != 2014 & Age == (j + 18))
		hld2 <- merge(hld, hld2, by = 'Name')
		do.mat[i,j] <- nrow(hld2)/nrow(hld)
	}
}

for(i in 19:41){
	nba$age.rnk[nba$Age == i] <-  rank(nba$CMB[nba$Age == i], na.last = "keep")/max(rank(nba$CMB[nba$Age == i], na.last = "keep"), na.rm=T)
	do <- data.frame('Age' = 19:44, 'cut' = 1 - do.mat[(i-18), ])
	imps <- subset(nba, Age == i & Season == 2014)
	for(j in 1:nrow(imps)){
		hld <- max(c(imps$Age[j], do$Age[do$cut <= imps$age.rnk[j]]), na.rm=T)
		nba$drop[nba$Name == imps$Name[j]] <- hld
	}
}
	

imps <- subset(nba, Season == 2014 & drop > Age)
for(i in 1:nrow(imps)){
	for(j in 1:(imps$drop[i] - imps$Age[i])){
		cur <- subset(nba, Age == imps$Age[i])
		 prj <- subset(nba, Age == imps$Age[i] + j)
		  adj <- merge(cur, prj, by = "Name")
		hld <- rank(c(imps$CMB[i], adj$CMB.x), na.last = "keep")[1]/max(rank(c(imps$CMB[i], adj$CMB.x), na.last = "keep"), na.rm=T)
		hld2 <- quantile(na.omit(adj$CMB.y), hld)
		nba <-rbind(nba, data.frame("Name"=imps$Name[i], "Season"=imps$Season[i]+j, "Age"=imps$Age[i]+j, "Pos"=imps$Pos[i], "CMB"=hld2, "drop"=NA, "age.rnk"=NA))
	}
}

nba <- nba[-c(6:7)]


##  mAX!!!

nba.mx <- aggregate( . ~ Name, data = nba, max, na.rm=T)

nba.mx$Pos[nba.mx$Pos == 0] <- NA



 #######

#clean and save
nba <- data.frame("Name" = nba.mx$Name, "Pos.nba"=nba.mx$Pos, "OBS" = nba.mx$CMB)

save(nba, file="nba")

