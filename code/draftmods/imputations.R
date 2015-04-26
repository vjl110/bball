rm(list=ls())
setwd("~/GitHub/bball")

###########
########

##
load("data/draftmod/ncaa_indi")
	ncaa <- ncaa[c(1:3, 6:20, 37:38, 44:49, 51:52, 54)]
tot <- read.csv("data/draftmod/BBRtot.csv")
	tot15 <- read.csv("data/draftmod/BBRtot2015.csv")
	pos <- rbind(tot, tot15)[c(1:2, 5)]
	tot <- rbind(tot, tot15)[c(1:2, 8:16, 18:23)]
		tot <- aggregate( . ~ Name + Season, data = tot, sum)
	tot <- tot[order(tot$Season), ]
	tot <- subset(tot, !duplicated(Name))
	colnames(tot) <- c("Name", "Season", "MP.nba", "X2P.nba", "X2PA.nba", "X3P.nba", "X3PA.nba", "FT.nba", "FTA.nba", "ORB.nba", "DRB.nba", "AST.nba", "STL.nba", "BLK.nba", "TOV.nba", "PF.nba", "PTS.nba")
	for(j in 4:ncol(tot)){
		tot[,j] <- tot[,j]/tot$MP.nba*40
	}
loc <- read.csv("data/draftmod/BBRloc.csv")[c(1:2, 9:23)]
	loc <- loc[order(loc$Season), ]
	loc <- subset(loc, !duplicated(Name))
hm <- read.csv("data/draftmod/hoopmath.csv")[c(1:2, 7:15)]
	hm$asd <- hm$atRIM*hm$asdRIM + hm$atJMP*hm$asdJMP + hm$X3PA*hm$asd3
	rmhld <- hm$atRIM 
	jphld <- hm$atJMP	
	hm$atRIM <- rmhld/(rmhld+jphld) # convert to % of 2s rather than of all shots
	hm$atJMP <- jphld/(rmhld+jphld)
	hm <- hm[-c(5, 8:11)]
dft <- read.csv("data/draftmod/Drafts.csv")[1:2]
load("data/draftmod/euro")
	euro <- data.frame(euro[1:2], "Tm"=NA, euro[c(4, 6:12)], "TRB"= euro$DRB + euro$ORB, euro[14:19], "MOV"=NA, "SOS"=NA, euro[c(37:42, 44:45, 47)], "atRIM"=NA, "RIM."=NA, "atJMP"=NA, "JMP."=NA, "asd"=NA)
load("lws.pick")

ncaa <- merge(ncaa, hm, by = c("Name", "Season"), all.x = T)
nba <- merge(tot, loc, by = c("Name", "Season"), all.x = T)[-2]
	ncaanba <- merge(euro, nba, by = c("Name"), all.x = T)
	euronba <- merge(ncaa, nba, by = c("Name"), all.x = T)
dat <- rbind(ncaanba, euronba) 	
dat <- merge(dat, dft, by = "Name", all.x = T)
	dat$Pick[is.na(dat$Pick)] <- 61
	dat$Pick <- predict(lws.pick, newdata=dat$Pick)

	rim1 <- lm(atRIM ~ 
		 per0.3 + I(X2PA/(X2PA+X3PA)) + I(X2P/X2PA) + Height +
		 I(FTA/(X2PA+X3PA)) + I(FT/FTA) + X3P + STL
		 , weights = MP, data = dat)
	rim2 <- lm(atRIM ~ 
		 I(X2PA/(X2PA+X3PA)) + I(X2P/X2PA) + Height +
		 I(FTA/(X2PA+X3PA)) + I(FT/FTA) + X3P + STL
		 , weights = MP, data = dat)
	asd1 <- lm(asd ~ 
		 Asd.2 + AST + I(X2PA/(X2PA+X3PA)) + I(FTA/(X2PA+X3PA)) + TRB + I(X2PA + X3PA) +
		 per0.3 + per10.16 + Height
		 , weights = MP, data = dat)
	asd2 <- lm(asd ~ 
		 AST + I(X2PA/(X2PA+X3PA)) + I(FTA/(X2PA+X3PA)) + TRB + I(X2PA + X3PA) + Height
		   , weights = MP, data = dat)

	nsvert <- lm(NS_vert ~ 
		 Height*Weight + Pick + MOV + SOS +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + PF + perDNK +
		 AST.nba + STL.nba + I(ORB.nba + DRB.nba)
		 , weights = ifelse(MP.nba < MP, MP.nba, MP), data = dat)
	nsvert.2 <- lm(NS_vert ~ 
		 Height*Weight + Pick +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + PF + perDNK +
		 AST.nba + STL.nba + I(ORB.nba + DRB.nba)
		 , weights = ifelse(MP.nba < MP, MP.nba, MP), data = dat)	
	nsvert.3 <- lm(NS_vert ~ 
		 Height*Weight + Pick + MOV + SOS +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + STL + BLK + TRB + PF
		 , weights = MP, data = dat)
	nsvert.4 <- lm(NS_vert ~ 
		 Height*Weight + Pick +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + STL + BLK + TRB + PF
		 , weights = MP, data = dat)	
	nsvert.5 <- lm(NS_vert ~ 
		 Height*Weight +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + STL + BLK + TRB + PF
		 , weights = MP, data = dat)		

	mxvert <- lm(max_vert ~ 
		 Height*Weight + Pick + MOV + SOS +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + perDNK +
		 AST.nba + STL.nba + BLK.nba + I(ORB.nba + DRB.nba) + PF.nba
		 , weights = ifelse(MP.nba < MP, MP.nba, MP), data = dat)
	mxvert.2 <- lm(max_vert ~ 
		 Height*Weight + Pick +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + perDNK +
		 AST.nba + STL.nba + BLK.nba + I(ORB.nba + DRB.nba) + PF.nba
		 , weights = ifelse(MP.nba < MP, MP.nba, MP), data = dat)	
	mxvert.3 <- lm(max_vert ~ 
		 Height*Weight + Pick + MOV + SOS +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + STL + BLK + TRB + PF
		 , weights = MP, data = dat)
	mxvert.4 <- lm(max_vert ~ 
		 Height*Weight + Pick +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + STL + BLK + TRB + PF
		 , weights = MP, data = dat)	
	mxvert.5 <- lm(max_vert ~ 
		 Height*Weight +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + STL + BLK + TRB + PF
		 , weights = MP, data = dat)	

	wings <- lm(Wingspan ~ 
		 Height*Weight + Pick + MOV + SOS +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + BLK + TOV +
		 AST.nba + STL.nba + BLK.nba + I(ORB.nba + DRB.nba) + TOV.nba + PF.nba
		 , weights = ifelse(MP.nba < MP, MP.nba, MP), data = dat)	
	wings.2 <- lm(Wingspan ~ 
		 Height*Weight + Pick +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + BLK + TOV +
		 AST.nba + STL.nba + BLK.nba + I(ORB.nba + DRB.nba) + TOV.nba + PF.nba
		 , weights = ifelse(MP.nba < MP, MP.nba, MP), data = dat)	
	wings.3 <- lm(Wingspan ~ 
		 Height*Weight + Pick + MOV + SOS +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + BLK +TOV +
		 STL + TRB + PF
		 , weights = MP, data = dat)
	wings.4 <- lm(Wingspan ~ 
		 Height*Weight + Pick +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + BLK +TOV +
		 STL + TRB + PF
		 , weights = MP, data = dat)
	wings.5 <- lm(Wingspan ~ 
		 Height*Weight +
		 AST + I(FT/FTA) + I(X2PA/(X2PA+X3PA)) + BLK +TOV +
		 STL + TRB + PF
		 , weights = MP, data = dat)	

	reach <- lm(Reach ~ 
		 Height*Weight + MOV + SOS +
		 AST + I(X2PA/(X2PA+X3PA)) + BLK +TOV + perDNK +
		 AST.nba + BLK.nba + ORB.nba + DRB.nba + TOV.nba + PF.nba
		 , weights = ifelse(MP.nba < MP, MP.nba, MP), data = dat)	
	reach.2 <- lm(Reach ~ 
		 Height*Weight +
		 AST + I(X2PA/(X2PA+X3PA)) + BLK +TOV + perDNK +
		 AST.nba + BLK.nba + ORB.nba + DRB.nba + TOV.nba + PF.nba
		 , weights = ifelse(MP.nba < MP, MP.nba, MP), data = dat)	
	reach.3 <- lm(Reach ~ 
		 Height*Weight + Pick + MOV + SOS +
		 AST + I(X2PA/(X2PA+X3PA)) + BLK + TOV + PF + TRB
		 , weights = MP, data = dat)
	reach.4 <- lm(Reach ~ 
		 Height*Weight + Pick +
		 AST + I(X2PA/(X2PA+X3PA)) + BLK + TOV + PF + TRB
		 , weights = MP, data = dat)	
	reach.5 <- lm(Reach ~ 
		 Height*Weight +
		 AST + I(X2PA/(X2PA+X3PA)) + BLK + TOV + PF + TRB
		 , weights = MP, data = dat)	

	sprnt <- lm(Sprint ~ 
		 Height*Weight + Pick +
		 TOV + perDNK +
		 AST.nba + TOV.nba + STL.nba
		 , weights = ifelse(MP.nba < MP, MP.nba, MP), data = dat)
	sprnt.2 <- lm(Sprint ~ 
		 Height*Weight + Pick +
		 FTA + I(X2PA/(X2PA+X3PA)) + AST + TOV + STL
		 , weights = MP, data = dat)
	sprnt.3 <- lm(Sprint ~ 
		 Height*Weight +
		 FTA + I(X2PA/(X2PA+X3PA)) + AST + TOV + STL
		 , weights = MP, data = dat)	

	aglty <- lm(Agility ~ 
		 Height*Weight + SOS + perDNK +
		 I(X2PA/(X2PA+X3PA)) + STL + BLK + PF
		 , weights = ifelse(MP.nba < MP, MP.nba, MP), data = dat)	
	aglty.2 <- lm(Agility ~ 
		 Height*Weight + perDNK +
		 I(X2PA/(X2PA+X3PA)) + STL + BLK + PF
		 , weights = ifelse(MP.nba < MP, MP.nba, MP), data = dat)	
	aglty.3 <- lm(Agility ~ 
		 Height*Weight + SOS +
		 I(X2PA/(X2PA+X3PA)) + STL + BLK + PF
		 , weights = MP, data = dat)
	aglty.4 <- lm(Agility ~ 
		 Height*Weight +
		 I(X2PA/(X2PA+X3PA)) + STL + BLK + PF
		 , weights = MP, data = dat)	


dat$atRIM[is.na(dat$atRIM)] <- predict(rim1, newdata=dat[is.na(dat$atRIM),])
	dat$atRIM[is.na(dat$atRIM)] <- predict(rim2, newdata=dat[is.na(dat$atRIM),])
	
dat$asd[is.na(dat$asd)] <- predict(asd1, newdata=dat[is.na(dat$asd),])
	dat$asd[is.na(dat$asd)] <- predict(asd2, newdata=dat[is.na(dat$asd),])
	
dat$NS_vert[is.na(dat$NS_vert)] <- predict(nsvert, newdata=dat[is.na(dat$NS_vert),])
	dat$NS_vert[is.na(dat$NS_vert)] <- predict(nsvert.2, newdata=dat[is.na(dat$NS_vert),])
		dat$NS_vert[is.na(dat$NS_vert)] <- predict(nsvert.3, newdata=dat[is.na(dat$NS_vert),])
			dat$NS_vert[is.na(dat$NS_vert)] <- predict(nsvert.4, newdata=dat[is.na(dat$NS_vert),])
				dat$NS_vert[is.na(dat$NS_vert)] <- predict(nsvert.5, newdata=dat[is.na(dat$NS_vert),])

dat$max_vert[is.na(dat$max_vert)] <- predict(mxvert, newdata=dat[is.na(dat$max_vert),])
	dat$max_vert[is.na(dat$max_vert)] <- predict(mxvert.2, newdata=dat[is.na(dat$max_vert),])
		dat$max_vert[is.na(dat$max_vert)] <- predict(mxvert.3, newdata=dat[is.na(dat$max_vert),])
			dat$max_vert[is.na(dat$max_vert)] <- predict(mxvert.4, newdata=dat[is.na(dat$max_vert),])
				dat$max_vert[is.na(dat$max_vert)] <- predict(mxvert.5, newdata=dat[is.na(dat$max_vert),])
	
dat$Wingspan[is.na(dat$Wingspan)] <- predict(wings, newdata=dat[is.na(dat$Wingspan),])
	dat$Wingspan[is.na(dat$Wingspan)] <- predict(wings.2, newdata=dat[is.na(dat$Wingspan),])
		dat$Wingspan[is.na(dat$Wingspan)] <- predict(wings.3, newdata=dat[is.na(dat$Wingspan),])
			dat$Wingspan[is.na(dat$Wingspan)] <- predict(wings.4, newdata=dat[is.na(dat$Wingspan),])
				dat$Wingspan[is.na(dat$Wingspan)] <- predict(wings.5, newdata=dat[is.na(dat$Wingspan),])

dat$Reach[is.na(dat$Reach)] <- predict(reach, newdata=dat[is.na(dat$Reach),])
	dat$Reach[is.na(dat$Reach)] <- predict(reach.2, newdata=dat[is.na(dat$Reach),])
		dat$Reach[is.na(dat$Reach)] <- predict(reach.3, newdata=dat[is.na(dat$Reach),])
			dat$Reach[is.na(dat$Reach)] <- predict(reach.4, newdata=dat[is.na(dat$Reach),])
				dat$Reach[is.na(dat$Reach)] <- predict(reach.5, newdata=dat[is.na(dat$Reach),])

dat$Sprint[is.na(dat$Sprint)] <- predict(sprnt, newdata=dat[is.na(dat$Sprint),])
	dat$Sprint[is.na(dat$Sprint)] <- predict(sprnt.2, newdata=dat[is.na(dat$Sprint),])
		dat$Sprint[is.na(dat$Sprint)] <- predict(sprnt.3, newdata=dat[is.na(dat$Sprint),])

dat$Agility[is.na(dat$Agility)] <- predict(aglty, newdata=dat[is.na(dat$Agility),])
	dat$Agility[is.na(dat$Agility)] <- predict(aglty.2, newdata=dat[is.na(dat$Agility),])
		dat$Agility[is.na(dat$Agility)] <- predict(aglty.3, newdata=dat[is.na(dat$Agility),])
			dat$Agility[is.na(dat$Agility)] <- predict(aglty.4, newdata=dat[is.na(dat$Agility),])


#### pos finder
	pos <- aggregate(. ~ Name, data = pos[-2], mean)
	pos$Pos <- as.factor(round(pos$Pos, 0))
	dat <- merge(dat, pos, by = "Name", all.x =T)
		pos.set <- subset(dat, !is.na(Pos))[c(1, 4:10, 12:18, 21:30, 34, 66)]
		pos.set.2 <- na.omit(pos.set)
	
	library(nnet)
	pos.mdl <- multinom(Pos ~ X2PA + X3PA + FT + FTA + TRB + AST + TOV + STL + BLK + PF + PTS + 
			    Height + Weight + NS_vert + max_vert + Reach + Wingspan + Agility + Age + atRIM + asd
			, data=dat, weights = MP)

	POS <- data.frame("PRB" = predict(pos.mdl, newdata=dat, "probs"))
	colnames(POS) <- c("PG", "SG", "SF", "PF", "C")
	POS <- na.omit(cbind("Name"=dat$Name, POS))
	POS <- aggregate(. ~ Name, data = POS, mean)
	for(i in 1:nrow(POS)){
		POS$Pos[i] <- round(POS$PG[i] + POS$SG[i]*2 + POS$SF[i]*3 + POS$PF[i]*4 + POS$C[i]*5, 2)
	}
POS <- POS[-c(2:6)]

#####################################	

SHOTLOC <- dat[c(1:4, 30, 34)]
ATH <- dat[c(1, 4, 23:28)]
	ATH <- na.omit(ATH)
		MPfinder <- ATH[1:2]
		MPfinder <- aggregate( . ~ Name, data = MPfinder, sum)
		colnames(MPfinder)[2] <- "mpdst"
		ATH <- merge(ATH, MPfinder, by = "Name", all.x=T)
	for(j in 3:8){
		ATH[,j] <- ATH[,j]*(ATH$MP/ATH$mpdst)
	}
	ATH <- aggregate( . ~ Name, data = ATH, sum)
	ATH <- ATH[-c(2,9)]

write.csv(SHOTLOC, "data/draftmod/SHOTLOC.csv", row.names=FALSE)	
write.csv(ATH, "data/draftmod/ATH.csv", row.names=FALSE)
write.csv(POS, "data/draftmod/POS.csv", row.names=FALSE)






