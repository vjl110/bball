setwd("C:/Users/Layne/Dropbox/bball")
rm(list=ls())


library(glmnet) #load glmnet package
library(SDMTools) #load glmnet package


###
###
INT.adv <- data.frame("Season" = 2001:2015, 
		       "EFG"=NA,  "FTR"=NA,  "TOR"=NA,  "REB"=NA, "MRG" = NA, "PACE" = NA) 
HOME.adv <- data.frame("Season" = 2001:2015, 
		       "EFG"=NA,  "FTR"=NA,  "TOR"=NA,  "REB"=NA, "MRG" = NA, "PACE" = NA) 
DIFF.adv <- data.frame("Season" = 2001:2015, 
		       "EFG"=NA,  "FTR"=NA,  "TOR"=NA,  "REB"=NA, "MRG" = NA, "PACE" = NA) 
DIFF_abs.adv <- data.frame("Season" = 2001:2015, 
		       "EFG"=NA,  "FTR"=NA,  "TOR"=NA,  "REB"=NA, "MRG" = NA, "PACE" = NA) 


DMP <- list()
for(s in 2001:2015){
	#####  Basic pre-pred that applies to all 4 factors
	lnp.yr <- subset(read.csv("data/LUP_RPM.csv", stringsAsFactors=FALSE), (Season == s | Season == s-1 | Season == s+1) & !is.na(EFG) & !is.na(FTR) & !is.na(TOR) & !is.na(REB) & POSS > 0 & T >= 0 & T < 100)
	plr.yr <- subset(read.csv("data/plr.csv", stringsAsFactors=FALSE), Season == s | Season == s-1 | Season == s+1)
		plr.yr <- subset(plr.yr, !duplicated(bbr))
		plr.yr <- plr.yr[order(plr.yr$bbr), ]
	lnp.yr[plr.yr$bbr] <- 0
	row.names(lnp.yr) <- 1:nrow(lnp.yr)
	O <- lnp.yr
	D <- lnp.yr
	for(j in 17:ncol(lnp.yr)){
		ego <- grep(colnames(O)[j], O$LINEUP)
		O[row.names(O) %in% ego, j] <- 1

		alt <- grep(colnames(D)[j], D$CP_LINEUP)
		D[row.names(D) %in% alt, j] <- 1
	}
	colnames(D) <- paste0(colnames(D), "_D")
	lnp.yr <- cbind(O, D[17:ncol(lnp.yr)])  ####  NEEED TO ADD A "D" to NAMEZ!!!
	rm(O)
	rm(D)

	data <- data.matrix(lnp.yr[c(4, 15:16,17:ncol(lnp.yr))]) #turn the data frame (which is now just 1s, -1s, and 0s) into a matrix
	#data <- data.matrix(lnp.yr[c(4,17:ncol(lnp.yr))]) #turn the data frame (which is now just 1s, -1s, and 0s) into a matrix
	X <- sparse.model.matrix(~data[,1]-1)
	for (i in 2:ncol(data)) {
		    coluna <- sparse.model.matrix(~data[,i]-1)
 			 X <- cBind(X, coluna)
	}

	#####  Begin modeling for each factor
	DV <- list(lnp.yr$EFG, lnp.yr$FTR, lnp.yr$TOR, lnp.yr$REB, lnp.yr$MRG/lnp.yr$POSS, lnp.yr$T)
	WGTs <- list(lnp.yr$FGA, lnp.yr$FGA, lnp.yr$POSS.to, lnp.yr$RB.chance, lnp.yr$POSS, lnp.yr$POSS)

	
	coef_O <- list()
	coef_D <- list()
	for(i in 1:length(DV)){
	dv <- DV[[i]]
	wgt <- WGTs[[i]]
	lambda <- cv.glmnet(X, dv, weights=wgt, alpha=0,) #find the lambda values. these determine how far towards 0 the coefficients are shrunk
	lambda.min <- lambda$lambda.min #store the lambda value that gives the smallest error in an object called lambda.min
	ridge <- glmnet(X, dv, family=c("gaussian"), wgt, alpha=0, lambda=lambda.min) #
		op <- coef(ridge ,s=lambda.min) #extract the coefficient for each of the independent variables (players) for the lambda with the minimum error 
			op <- as.data.frame(as.matrix(op))
				op$ID <- c("INT", colnames(data))
					colnames(op) <- c("df", "bbr")
					INT.adv[s-2000, i+1] <- op[1,1]
					HOME.adv[s-2000, i+1] <- op[2,1]
					DIFF.adv[s-2000, i+1] <- op[3,1]
					DIFF_abs.adv[s-2000, i+1] <- op[4,1]
						row.names(op) <- 1:nrow(op)
						op_O <- op[-c(grep("_D", op[,2])), ]
						op_D <- op[c(grep("_D", op[,2])), ]
							op_D[,2] <- gsub("_D", "", op_D[,2])
							op_O <- na.omit(merge(op_O, plr.yr, by = "bbr", all.x=T))
							op_D <- na.omit(merge(op_D, plr.yr, by = "bbr", all.x=T))

	coef_O[[i]] <- op_O
	coef_D[[i]] <- op_D
	}
	rm(X)
	rm(op)
	rm(op_O)
	rm(op_D)


	output <- data.frame("name" = coef_O[[1]]$name, "id" = coef_O[[1]]$id, "Season" = s, 
			     "EFG.O"=coef_O[[1]]$df, "EFG.D"=coef_D[[1]]$df,
			     "FTR.O"=coef_O[[2]]$df, "FTR.D"=coef_D[[2]]$df,
			     "TOR.O"=coef_O[[3]]$df, "TOR.D"=coef_D[[3]]$df,
			     "REB.O"=coef_O[[4]]$df, "REB.D"=coef_D[[4]]$df,
			     "MRG.O"=coef_O[[5]]$df, "MRG.D"=coef_D[[5]]$df,
			     "PCE.O"=coef_O[[6]]$df, "PCE.D"=coef_D[[6]]$df)
	DMP[[s - 2000]] <- output

		rm(output)
		rm(coef_O)
		rm(coef_D)
}



NPI <- do.call("rbind", DMP)
	TOT <- read.csv("data/TOT_pull.csv")
		TOTpo <- read.csv("data/TOTPO_pull.csv")
		TOT <- rbind(TOT, TOTpo)
		TOT <- subset(TOT, Tm != "TOT")[-c(3:5)]
		TOT$Season <- as.numeric(substr(TOT$Season, 1, 4)) + 1
		TOT <- aggregate(. ~ Season + id + Age, TOT, sum)
		TOT <- TOT[c(1, 2, 6)]	
			NPI <- merge(NPI, TOT, by = c("id", "Season"), all.x = T)
NPI <- subset(NPI, !is.na(MP))
write.csv(NPI, "NPI.csv", row.names = F)

write.csv(INT.adv, "INT.adv.csv", row.names = F)
write.csv(HOME.adv, "HOME.adv.csv", row.names = F)
write.csv(DIFF.adv, "DIFF.adv.csv", row.names = F)
write.csv(DIFF_abs.adv, "DIFF_abs.csv", row.names = F)


###
########

#  BUILD MODELS FOR PRIORS

########
###

INT.adv <- read.csv("INT.adv.csv")
HOME.adv <- read.csv("HOME.adv.csv")
DIFF.adv <- read.csv("DIFF.adv.csv")
DIFF_abs.adv <- read.csv("DIFF_abs.csv")
NPI <- read.csv("NPI.csv")

#########  4FBPM

####
####	Statistical Prior for NBA players

TOT <- read.csv("data/TOT_pull.csv")
	TOTpo <- read.csv("data/TOTPO_pull.csv")
	TOT <- rbind(TOT, TOTpo)
	TOT <- subset(TOT, Tm != "TOT")[-c(3:5)]
	TOT$Season <- as.numeric(substr(TOT$Season, 1, 4)) + 1
	TOT <- aggregate(. ~ Season + id + Age, TOT, sum)
ADV <- read.csv("data/ADV_pull.csv")
	ADVpo <- read.csv("data/ADVPO_pull.csv")
	ADV <- rbind(ADV, ADVpo)
	ADV <- subset(ADV, Tm != "TOT")[-c(3:5)]
	ADV[c(5:13, 16:18)] <- ADV[c(5:13, 16:18)]*ADV$MP
	ADV$Season <- as.numeric(substr(ADV$Season, 1, 4)) + 1
	ADV <- aggregate(. ~ Season + id + Age, ADV, sum)
	ADV[c(6:14, 17:19)] <- ADV[c(6:14, 17:19)]/ADV$MP
SHT <- read.csv("data/SHT_pull.csv")
	SHTpo <- read.csv("data/SHTPO_pull.csv")
	SHT <- rbind(SHT, SHTpo)
	SHT <- subset(SHT, Tm != "TOT")[-c(3:5)]
		SHT[c(11:17, 19:21)][is.na(SHT[c(11:17, 19:21)])] <- 0
	SHT[c(5:17, 19:21)] <- SHT[c(5:17, 19:21)]*SHT$MP
	SHT$Season <- as.numeric(substr(SHT$Season, 1, 4)) + 1
	SHT <- aggregate(. ~ Season + id + Age, SHT, sum)
	SHT[c(6:18, 20:22)] <- SHT[c(6:18, 20:22)]/SHT$MP
PBP <- read.csv("data/PBP_pull.csv")
	PBPpo <- read.csv("data/PBPPO_pull.csv")
		PBPpo <- subset(PBPpo, Season != "2014-15") # Necessary until they enter
	PBP <- rbind(PBP, PBPpo)
	PBP <- subset(PBP, Tm != "TOT")[-c(3:5)]
	for(j in 5:9){
		PBP[,j] <- as.numeric(sub("%", "e-2", PBP[,j]))
	}
	PBP[,5:9][is.na(PBP[,5:9])] <- 0
	PBP$POS <- PBP$PG. + 2*PBP$SG. + 3*PBP$SF. + 4*PBP$PF. + 5*PBP$C.		
	PBP <- PBP[-c(5:9)]
	PBP[c(5:6, 19)] <- PBP[c(5:6, 19)]*PBP$MP
	PBP$Block <- NULL   # too many NAs in these two
	PBP$Take <- NULL
	PBP$Season <- as.numeric(substr(PBP$Season, 1, 4)) + 1
	PBP <- aggregate(. ~ Season + id + Age, PBP, sum)
	PBP[c(6:7, 17)] <- PBP[c(6:7, 17)]/PBP$MP

STAT <- merge(TOT, ADV[-c(3:5)], by = c("id", "Season"), all.x = T)
STAT <- merge(STAT, SHT[-c(3:5)], by = c("id", "Season"), all.x = T)
STAT <- merge(STAT, PBP[-c(3:5)], by = c("id", "Season"), all.x = T)

for(j in 37:41){
	STAT[,j] <- round(STAT[,j]*(STAT$X2PA + STAT$X3PA), 0)
	STAT[,j+5] <- round(STAT[,j+5]*STAT[,j], 0)
}
STAT$crnr <- round(STAT$tre*STAT$crnr, 0)
STAT$crnr. <- round(STAT$crnr*STAT$crnr., 0)
STAT$asd2 <- STAT$X2PA*STAT$asd2
STAT$asd3 <- STAT$X3PA*STAT$asd3
STAT$dnk <- NULL

NPI <- merge(NPI, STAT, by = c("id", "Season"), all.x = T)


### Switch to STepAIC and fix defense issues..

# O models
efgo <- lm(EFG.O ~ 
		I(rim/MP.y) + I(rim./MP.y) + I(mid/MP.y) + I(mid./MP.y) + I(lng/MP.y) + I(lng./MP.y) +
		I(crnr/MP.y) + I(crnr./MP.y) + I((tre-crnr)/MP.y) + I((tre.-crnr.)/MP.y) +
		PCE.O + PCE.D +
		AST. +
                I(FT/(X2PA + X3PA)) +
		STL. +
                I((MP.y/G)^2) + as.numeric(Age) + I(as.numeric(Age)^2) + POS, 
           data = NPI, weights = MP.x)
NPI$efgo <- predict(efgo, newdat = NPI) 

ftro <- lm(FTR.O ~ 
		I(rim/MP.y) + I(mid/MP.y) + I(lng/MP.y) + 
		I(crnr/MP.y) + I((tre-crnr)/MP.y) +
		PCE.O + PCE.D +
		AST. + TOV. + I(asd2/MP.y) + I(asd3/MP.y) +
                I(FTA/MP.y) +
                I((MP.y/G)^2) + as.numeric(Age) + I(as.numeric(Age)^2) + POS, 
           data = NPI, weights = MP.x)
NPI$ftro <- predict(ftro, newdat = NPI) 

toro <- lm(TOR.O ~ 
		I(rim/MP.y) + I(mid/MP.y) + I(lng/MP.y) + 
		I(crnr/MP.y) + I((tre-crnr)/MP.y) +
		PCE.D +
		AST. + TOV. + I(asd2/MP.y) + I(Offens/MP.y) + 
                I(MP.y/G) + I((MP.y/G)^2) + POS, 
		data = NPI, weights = MP.x)
NPI$toro <- predict(toro, newdat = NPI) 

rebo <- lm(REB.O ~ 
		I(rim/MP.y) + I(rim./MP.y) + I(mid/MP.y) + I(mid./MP.y) + I(lng/MP.y) + I(lng./MP.y) +
		I(crnr/MP.y) + I(crnr./MP.y) + I((tre-crnr)/MP.y) + I((tre.-crnr.)/MP.y) +
		PCE.O + PCE.D +
                I(FTA/MP.y) +
		ORB.*DRB. +
                I(MP.y/G) + I((MP.y/G)^2) + POS, 
           data = NPI, weights = MP.x)
NPI$rebo <- predict(rebo, newdat = NPI) 

mrgo <- lm(MRG.O ~ efgo + ftro + toro + rebo
		 , data = NPI, weights = MP.x)	
NPI$mrgo <- predict(mrgo, newdat = NPI) 

pceo <- lm(PCE.O ~ 
		I(rim/MP.y) + I(rim./MP.y) + I(mid/MP.y) + I(mid./MP.y) + I(lng/MP.y) + I(lng./MP.y) +
		I(crnr/MP.y) + I(crnr./MP.y) + I((tre-crnr)/MP.y) + I((tre.-crnr.)/MP.y) +
		AST.*TOV. + I(LostBall/MP.y) + I(BadPass/MP.y) + I(Offens/MP.y) +
                I(FTA/(X2PA + X3PA + AST + TOV)) +
		ORB.*DRB. +
		STL. +
                I(MP.y/G) + I((MP.y/G)^2) + as.numeric(Age) + I(as.numeric(Age)^2) + POS, 
           data = NPI, weights = MP.x)
NPI$pceo <- predict(pceo, newdat = NPI) 

# D models
efgd <- lm(EFG.D ~ 
		PCE.O + PCE.D +
		AST.*TOV. +
		STL. + BLK.*I((PF-Offens)/MP.y) +
		ORB.*POS + DRB. +
                I(MP.y/G) + as.numeric(Age) + I(as.numeric(Age)^2) + POS, 
           data = NPI, weights = MP.x)
NPI$efgd <- predict(efgd, newdat = NPI) 

ftrd <- lm(FTR.D ~ 
		PCE.O + PCE.D +
		STL. + BLK. +
		I((PF-Offens-Shoot)/MP.y) + I(Shoot/MP.y) +
                I(MP.y/G) + I((MP.y/G)^2) + POS, 
           data = NPI, weights = MP.x)
NPI$ftrd <- predict(ftrd, newdat = NPI) 

tord <- lm(TOR.D ~ 
		PCE.O + PCE.D +
		STL. + BLK. +
		I(Shoot/MP.y) + I((PF-Offens-Shoot)/MP.y) + 
                as.numeric(Age) + I(as.numeric(Age)^2) + POS, 
           data = NPI, weights = MP.x)
NPI$tord <- predict(tord, newdat = NPI) 

rebd <- lm(REB.D ~ 
		PCE.O + PCE.D +
		DRB. + ORB. +
		BLK. + STL. +
		I((PF-Offens)/MP.y) +
                I(MP.y/G) + I((MP.y/G)^2) + as.numeric(Age) + I(as.numeric(Age)^2), 
           data = NPI, weights = MP.x)
NPI$rebd <- predict(rebd, newdat = NPI) 

mrgd <- lm(MRG.D ~ efgd + ftrd + tord + rebd
		   , data = NPI, weights = MP.x)		
NPI$mrgd <- predict(mrgd, newdat = NPI) 

pced <- lm(PCE.D ~ 
		ORB.*DRB. +
		BLK. +
		I((PF-Offens-Shoot)/MP.y) + I((Shoot)/MP.y) +
                I(MP.y/G) + I((MP.y/G)^2) + as.numeric(Age) + I(as.numeric(Age)^2) + POS, 
           data = NPI, weights = MP.x)
NPI$pced <- predict(pced, newdat = NPI) 

PRIOR <- na.omit(NPI[c(1:3, 80:ncol(NPI))])

#########  Project for rookies #######3
PLR <- read.csv("data/plr.csv", stringsAsFactors=FALSE)
	PLR <- PLR[order(PLR$Season), ]
	ROOK <- subset(PLR, !duplicated(id))
NPI <- read.csv("NPI.csv")
RK.prd <- merge(ROOK, NPI, by = c("id", "name", "Season"))
RK.prd <- RK.prd[c(1:3, 5, 7, 9, 11, 13, 15, 6, 8, 10, 12, 14, 16)]

	BLND <- merge(PRIOR, NPI[c(1:2, 16)], by = c("id", "Season"), all.x = T)
		pst <- BLND
			pst$Season <- pst$Season - 1
		pre <- BLND 
		prepst <- merge(pre, pst, by = c("id", "name", "Season"), all.y = T)
			prepst$Season <- prepst$Season + 1
			ROOK$RK <- 1
		prepst <- merge(prepst, ROOK[c(1:2, 5)], by = c("id", "Season"), all.x = T)
		prepst$RK[is.na(prepst$RK)] <- 0
		for(j in 4:15){
			prepst[prepst$RK == 1, j] <- mean(RK.prd[,j], na.rm=T)
		}
		prepst$MP.x[prepst$RK == 1] <- 500
		
for(j in 4:15){
	PRIOR[, j] <- ifelse(is.na(prepst$MP.x), prepst[,j+13], (prepst[,j]*prepst$MP.x + prepst[,j+13]*prepst$MP.y)/(prepst$MP.y + prepst$MP.x))
}


######    WITH PRIORS!!!!!
DMP <- list()
for(s in 2001:2015){
	#####  Basic pre-pred that applies to all 4 factors
	lnp.yr <- subset(read.csv("data/LUP_RPM.csv", stringsAsFactors=FALSE), (Season == s | Season == s-1) & !is.na(EFG) & !is.na(FTR) & !is.na(TOR) & !is.na(REB) & POSS > 0 & T >= 0 & T < 100)
	plr.yr <- subset(read.csv("data/plr.csv", stringsAsFactors=FALSE), (Season == s | Season == s-1))
		plr.yr <- subset(plr.yr, !duplicated(bbr))
		plr.yr <- plr.yr[order(plr.yr$bbr), ]
			plr.yr <- merge(plr.yr, PRIOR, by = c("id", "Season", "name"), all.x = T)
				for(j in 5:16){
					plr.yr[,j][is.na(plr.yr[,j])] <- mean(plr.yr[,j], na.rm=T)
				}
	lnp.yr[plr.yr$bbr] <- 0
	row.names(lnp.yr) <- 1:nrow(lnp.yr)
	O <- lnp.yr
	D <- lnp.yr
	for(j in 17:ncol(lnp.yr)){
		ego <- grep(colnames(O)[j], O$LINEUP)
		O[row.names(O) %in% ego, j] <- 1

		alt <- grep(colnames(D)[j], D$CP_LINEUP)
		D[row.names(D) %in% alt, j] <- 1
	}
	colnames(D) <- paste0(colnames(D), "_D")
	lnp.yr <- cbind(O, D[17:ncol(lnp.yr)])  ####  NEEED TO ADD A "D" to NAMEZ!!!
	rm(O)
	rm(D)

	data <- data.matrix(lnp.yr[c(4, 15:16,17:ncol(lnp.yr))]) #turn the data frame (which is now just 1s, -1s, and 0s) into a matrix
	#data <- data.matrix(lnp.yr[c(4,17:ncol(lnp.yr))]) #turn the data frame (which is now just 1s, -1s, and 0s) into a matrix
	X <- sparse.model.matrix(~data[,1]-1)
	for (i in 2:ncol(data)) {
		    coluna <- sparse.model.matrix(~data[,i]-1)
 			 X <- cBind(X, coluna)
	}

	#####  Begin modeling for each factor
	DV <- list(lnp.yr$EFG, lnp.yr$FTR, lnp.yr$TOR, lnp.yr$REB, lnp.yr$MRG/lnp.yr$POSS, lnp.yr$T)
	WGTs <- list(lnp.yr$FGA, lnp.yr$FGA, lnp.yr$POSS.to, lnp.yr$RB.chance, lnp.yr$POSS, lnp.yr$POSS)
	OFF_o <- list(plr.yr[,c(4,5)], plr.yr[,c(4,6)], plr.yr[,c(4,7)], plr.yr[,c(4,8)], plr.yr[,c(4,9)], plr.yr[,c(4,10)])
	OFF_d <- list(plr.yr[,c(4,11)], plr.yr[,c(4,12)], plr.yr[,c(4,13)], plr.yr[,c(4,14)], plr.yr[,c(4,15)], plr.yr[,c(4,16)])
	ADJ <- list(mean(HOME.adv$EFG)*lnp.yr$HOME + mean(DIFF.adv$EFG)*lnp.yr$DIF + mean(DIFF_abs.adv$EFG)*lnp.yr$DIF_abs, 
			mean(HOME.adv$FTR)*lnp.yr$HOME + mean(DIFF.adv$FTR)*lnp.yr$DIF + mean(DIFF_abs.adv$FTR)*lnp.yr$DIF_abs, 
				mean(HOME.adv$TOR)*lnp.yr$HOME + mean(DIFF.adv$TOR)*lnp.yr$DIF + mean(DIFF_abs.adv$TOR)*lnp.yr$DIF_abs, 
					mean(HOME.adv$REB)*lnp.yr$HOME + mean(DIFF.adv$REB)*lnp.yr$DIF + mean(DIFF_abs.adv$REB)*lnp.yr$DIF_abs, 
						mean(HOME.adv$MRG)*lnp.yr$HOME + mean(DIFF.adv$MRG)*lnp.yr$DIF + mean(DIFF_abs.adv$MRG)*lnp.yr$DIF_abs, 
							mean(HOME.adv$PACE)*lnp.yr$HOME + mean(DIFF.adv$PACE)*lnp.yr$DIF + mean(DIFF_abs.adv$PACE)*lnp.yr$DIF_abs)
	coef_O <- list()
	coef_D <- list()
	for(i in 1:length(DV)){
		os <- lnp.yr[17:ncol(lnp.yr)]
			for(j in 1:(ncol(os)/2)){
				os[,j][os[,j] == 1] <- OFF_o[[i]][,2][OFF_o[[i]][,1] == colnames(os)[j]]
			}
			for(j in (ncol(os)/2+1):ncol(os)){
				os[,j][os[,j] == 1] <- OFF_d[[i]][,2][paste0(OFF_d[[i]][,1], "_D") == colnames(os)[j]]
			}
		os <- matrix(rowSums(os, na.rm=T))
		os <- os + ADJ[[i]]	
	dv <- DV[[i]]
	wgt <- WGTs[[i]]
	lambda <- cv.glmnet(X, dv-os, weights=wgt, alpha=0) #find the lambda values. these determine how far towards 0 the coefficients are shrunk
	lambda.min <- lambda$lambda.min #store the lambda value that gives the smallest error in an object called lambda.min
	ridge <- glmnet(X, dv-os, family=c("gaussian"), wgt, alpha=0, lambda=lambda.min) #
		op <- coef(ridge, s=lambda.min) #extract the coefficient for each of the independent variables (players) for the lambda with the minimum error 
			op <- as.data.frame(as.matrix(op))
				op$ID <- c("INT", colnames(data))
					colnames(op) <- c("df", "bbr")
						row.names(op) <- 1:nrow(op)
						op_O <- op[-c(grep("_D", op[,2])), ]
						op_D <- op[c(grep("_D", op[,2])), ]
							op_D[,2] <- gsub("_D", "", op_D[,2])

							op_O <- na.omit(merge(op_O, plr.yr, by = "bbr", all.x=T))
							op_D <- na.omit(merge(op_D, plr.yr, by = "bbr", all.x=T))

	coef_O[[i]] <- op_O
	coef_D[[i]] <- op_D
	}
	rm(X)
	rm(op)
	rm(op_O)
	rm(op_D)


	output <- data.frame("name" = coef_O[[1]]$name, "id" = coef_O[[1]]$id, "Season" = s, 
			     "EFG.O"=coef_O[[1]]$df, "FTR.O"=coef_O[[2]]$df, "TOR.O"=coef_O[[3]]$df, "REB.O"=coef_O[[4]]$df, "MRG.O"=coef_O[[5]]$df, "PCE.O"=coef_O[[6]]$df,
			     "EFG.D"=coef_D[[1]]$df, "FTR.D"=coef_D[[2]]$df, "TOR.D"=coef_D[[3]]$df, "REB.D"=coef_D[[4]]$df, "MRG.D"=coef_D[[5]]$df, "PCE.D"=coef_D[[6]]$df)	
	DMP[[s - 2000]] <- output

		rm(output)
		rm(coef_O)
		rm(coef_D)
}


PI <- do.call("rbind", DMP)
	TOT <- read.csv("data/TOT_pull.csv")
		TOTpo <- read.csv("data/TOTPO_pull.csv")
		TOT <- rbind(TOT, TOTpo)
		TOT <- subset(TOT, Tm != "TOT")[-c(3:5)]
		TOT$Season <- as.numeric(substr(TOT$Season, 1, 4)) + 1
		TOT <- aggregate(. ~ Season + id + Age, TOT, sum)
		TOT <- TOT[c(1, 2, 6)]	
			PI <- merge(PI, TOT, by = c("id", "Season"), all.x = T)
PI <- subset(PI, !is.na(MP))
write.csv(PI, "PI.csv", row.names = F)


PIPRIOR <- merge(PI[c(1:3, 16, 4:15)], PRIOR, by = c("id", "name", "Season"), all.x = T)	
OUTPUT <- data.frame("id"=PIPRIOR$id, "name"=PIPRIOR$name, "Season"=PIPRIOR$Season, "MP"=PIPRIOR$MP, "EFGO"=NA, "FTRO"=NA, "TORO"=NA, "REBO"=NA, "MRGO"=NA, "PCEO"=NA, "EFGD"=NA, "FTRD"=NA, "TORD"=NA, "REBD"=NA, "MRGD"=NA, "PCED"=NA) 
	for(j in 5:16){
		OUTPUT[,j] <- round(PIPRIOR[,j] + PIPRIOR[,j+12], 3)
}
OUTPUT$MRGO <- OUTPUT$MRGO*100
OUTPUT$MRGD <- OUTPUT$MRGD*100
write.csv(OUTPUT, "OUTPUT.csv", row.names = F)










	for(j in 4:15){
		NPI[,j] <- round(NPI[,j], 3)
	}
NPI$MRG.O <- NPI$MRG.O*100
NPI$MRG.D <- NPI$MRG.D*100
write.csv(NPI, "OUTPUT.csv", row.names = F)






for(j in 5:16){
	OUTPUT[,j] <- round(OUTPUT[,j], 3)
}
OUTPUT$MRGO <- OUTPUT$MRGO*100
OUTPUT$MRGD <- OUTPUT$MRGD*100
write.csv(OUTPUT, "OUTPUT.csv", row.names = F)

one <- OUTPUT
	one$Season <- one$Season + 2
two <- OUTPUT
	two$Season <- two$Season + 1
	two$MP <- NULL
tre <- OUTPUT
	tre$MP <- NULL

tst <- merge(one, two, by  = c("id", "name", "Season"), all.x = T)
tst <- merge(tst, tre, by  = c("id", "name", "Season"), all.x = T)

tst <- na.omit(tst)	


m1 <- lm(MRGO ~ MRGO.x + MRGO.y, data = OUTPUT)




PI <- read.csv("PI.csv")
tst <- merge(PI, PRIOR, by = c("id", "name", "Season"), all.x = T)

pre <- tst
	pre$Season <- pre$Season + 1
pst <- tst
	pst$MP <- NULL

retst <- merge(pre, pst, by  = c("id", "name", "Season"), all.x = T)
retst <- na.omit(retst)	


hld <- c()
for(i in 1:100){
	pre <- scale(retst$EFG.O.x)*(0.01*i) + scale(retst$efgo.x)*(0.01*(100-i))
	pst <- scale(retst$EFG.O.y)*(0.01*i) + scale(retst$efgo.y)*(0.01*(100-i)) 
	hld[i] <- cor(pre, pst)
}






















#####
	# Run single-year
######

DMP <- list()
for(s in 2001:2015){
	#####  Basic pre-pred that applies to all 4 factors
	lnp.yr <- subset(read.csv("LUP_RPM.csv", stringsAsFactors=FALSE), Season == s & !is.na(EFG.df) & !is.na(FT.df) & !is.na(TOV.df) & !is.na(REB.df) & POSS > 0)
		lnp.yr <- lnp.yr[order(lnp.yr$HOME, decreasing = T), ]
		hm.brk <- nrow(subset(lnp.yr, HOME == 1))
	plr.yr <- subset(read.csv("PLR_RPM.csv", stringsAsFactors=FALSE), Season == s)
		plr.yr <- plr.yr[,c(1:2, ncol(plr.yr))]
			plr.yr <- subset(plr.yr, !duplicated(ID))
	lnp.yr[plr.yr$ID] <- 0
	row.names(lnp.yr) <- 1:nrow(lnp.yr)
	for(j in 24:ncol(lnp.yr)){
		ego <- grep(colnames(lnp.yr)[j], lnp.yr$LINEUP)
		alt <- grep(colnames(lnp.yr)[j], lnp.yr$CP_LINEUP)
		lnp.yr[row.names(lnp.yr) %in% ego, j] <- 1
		lnp.yr[row.names(lnp.yr) %in% alt, j] <- -1
	}
	
	data <- data.matrix(lnp.yr[c(6,24:ncol(lnp.yr))]) #turn the data frame (which is now just 1s, -1s, and 0s) into a matrix
	X <- sparse.model.matrix(~data[,1]-1)
	for (i in 2:ncol(data)) {
		    coluna <- sparse.model.matrix(~data[,i]-1)
 			 X <- cBind(X, coluna)
	}
	X2 <- X
	X2[X2 == -1] <- 1

	#####  Begin modeling for each factor
	DV_DFs <- list(lnp.yr$EFG.df, lnp.yr$FT.df, lnp.yr$TOV.df, lnp.yr$REB.df, lnp.yr$MRG.df/lnp.yr$POSS)
	DV_ODs <- list(lnp.yr$EFG.od, lnp.yr$FT.od, lnp.yr$TOV.od, lnp.yr$REB.od, lnp.yr$MRG.od/lnp.yr$POSS)
	WGTs <- list(lnp.yr$FGA, lnp.yr$FGA, lnp.yr$POSS.to, lnp.yr$RB.chance, lnp.yr$POSS)

	coef_hld <- list()
	for(i in 1:length(DV_DFs)){		

	dv_df <- DV_DFs[[i]]
	dv_od <- DV_ODs[[i]]
	wgt <- WGTs[[i]]
	lambda <- cv.glmnet(X, dv_df, weights=wgt, alpha=0) #find the lambda values. these determine how far towards 0 the coefficients are shrunk
	lambda.1se <- lambda$lambda.1se #store the lambda value that gives the smallest error in an object called lambda.min
	ridge <- glmnet(X, dv_df, family=c("gaussian"), wgt, alpha=0, lambda=lambda.1se) #
		op <- coef(ridge ,s=lambda.1se) #extract the coefficient for each of the independent variables (players) for the lambda with the minimum error 
			op <- as.data.frame(as.matrix(op))
				op$ID <- c("INT", colnames(data))
					colnames(op) <- c("df", "ID")
						row.names(op) <- 1:nrow(op)
							op_df <- na.omit(merge(op, plr.yr[1:3], by = "ID", all.x=T))

	lambda <- cv.glmnet(X2, dv_od, weights=wgt, alpha=0) #find the lambda values. these determine how far towards 0 the coefficients are shrunk
	lambda.1se <- lambda$lambda.1se #store the lambda value that gives the smallest error in an object called lambda.min
	ridge <- glmnet(X2, dv_od, family=c("gaussian"), wgt, alpha=0, lambda=lambda.1se) #
		op <- coef(ridge ,s=lambda.1se) #extract the coefficient for each of the independent variables (players) for the lambda with the minimum error 
			op <- as.data.frame(as.matrix(op))
				op$ID <- c("INT", colnames(data))
					colnames(op) <- c("od", "ID")
						row.names(op) <- 1:nrow(op)
							op_od <- na.omit(merge(op, plr.yr[1:3], by = "ID", all.x=T))
			
	op <- merge(op_df[-3], op_od[-3], by = c( "ID", "Name"))
	coef_hld[[i]] <- op
	}
	rm(X)
	rm(X2)	
	
	output <- data.frame("Name" = coef_hld[[1]]$Name, "ID" = coef_hld[[1]]$ID,
			     "EFG.O"=(coef_hld[[1]]$df + coef_hld[[1]]$od)/2, 
			     "EFG.D"=(coef_hld[[1]]$od - coef_hld[[1]]$df)/2, 
			     "FT.O"=(coef_hld[[2]]$df + coef_hld[[2]]$od)/2, 
			     "FT.D"=(coef_hld[[2]]$od - coef_hld[[2]]$df)/2,
			     "TOV.O"=(coef_hld[[3]]$df + coef_hld[[3]]$od)/2, 
			     "TOV.D"=(coef_hld[[3]]$od - coef_hld[[3]]$df)/2, 
			     "REB.O"=(coef_hld[[4]]$df + coef_hld[[4]]$od)/2, 
			     "REB.D"=(coef_hld[[4]]$od - coef_hld[[4]]$df)/2, 
			     "MRG.O"=(coef_hld[[5]]$df + coef_hld[[5]]$od)/2, 
			     "MRG.D"=(coef_hld[[5]]$od - coef_hld[[5]]$df)/2)
			rm(coef_hld)
		plr.set <- subset(read.csv("PLR_RPM.csv", stringsAsFactors=FALSE), Season == s)[c(1, 9, 61)]
		plr.set <- aggregate( . ~ Name + ID, data = plr.set, sum)
	output <- na.omit(merge(plr.set, output, by = c("Name", "ID"), all.x = T))
	output <- subset(output, !duplicated(Name))
	output$Season <- s
	DMP[[s - 2000]] <- output
		rm(output)
}

BPM <- do.call("rbind", DMP)
write.csv(BPM, "BPM.csv", row.names = F)


############  PREPARE FOR PUB


# PLAYER INFO
	plr <- read.csv("PLR_RPM.csv", stringsAsFactors=FALSE)
	plr <- subset(plr, Season >= 2001)
	plr <- plr[c(61, 1:3, 9)]
	for(s in 2001:2015){
		plrz <- unique(plr$ID[plr$Season == s])
		for(i in 1:length(plrz)){
			plr$Tm[plr$ID == plrz[i] & plr$Season == s] <- 
				paste(sort(unique(plr$Tm[plr$ID == plrz[i] & plr$Season == s])), collapse = ", ")
		}	
	}
	plr <- aggregate( . ~ Name + ID + Tm + Season, data = plr, sum)

#  NPI output
NPI <- read.csv("NPI.csv")[c(1:2,14,4:13)]
	NPI <- merge(NPI, plr, by = c("Name", "ID", "Season"))
	NPI <- NPI[c(1, 14, 3, 15, 4:13)]
	for(j in c(6, 8, 9, 12, 14)){
		NPI[,j] <- -NPI[,j]
	}
NPI$EFG <- NPI$EFG.O + NPI$EFG.D
NPI$FT <- NPI$FT.O + NPI$FT.D
NPI$TOV <- NPI$TOV.O + NPI$TOV.D
NPI$REB <- NPI$REB.O + NPI$REB.D
NPI$NET <-  NPI$MRG.O + NPI$MRG.D

#for(j in 5:19){
#	NPI <- NPI[order(NPI[,j]), ]
#	NPI[,j] <- round((1:nrow(NPI))/nrow(NPI), 2)
#}

#BPM output
	PRIOR <- merge(PRIOR, plr, by = c("Name", "ID", "Season"))
	PRIOR <- PRIOR[c(1, 14, 3, 15, 4:13)]
	for(j in c(6, 8, 9, 12, 14)){
		PRIOR[,j] <- -PRIOR[,j]
	}
PRIOR$EFG <- PRIOR$efgo + PRIOR$efgd
PRIOR$FT <- PRIOR$fto + PRIOR$ftd
PRIOR$TOV <- PRIOR$tovo + PRIOR$tovd
PRIOR$REB <- PRIOR$rebo + PRIOR$rebd
PRIOR$NET <-  PRIOR$mrgo + PRIOR$mrgd

colnames(PRIOR) <- colnames(NPI)

#for(j in 5:19){
#	PRIOR <- PRIOR[order(PRIOR[,j]), ]
#	PRIOR[,j] <- round((1:nrow(PRIOR))/nrow(PRIOR), 2)
#}



######
####### Hybrid
hld <- merge(NPI, PRIOR, by = c("Name", "Tm", "Season", "MP"), all.x = T)
for(j in c(13:14, 19, 28:29, 34)){
	hld[,j] <- hld[,j]*100
}
for(j in 20:34){
	avg <- (mean(hld[,j], na.rm=T) + mean(hld[,j-15], na.rm=T))/2
	std <- (sd(hld[,j-15], na.rm=T) + sd(hld[,j], na.rm=T))/2	
	hld[,j-15] <- scale(hld[,j-15])
		hld[,j-15] <- avg + std*hld[,j-15]
}

for(j in 5:19){
	hld[,j] <- ifelse(hld$MP < 2000, hld[,j]*(hld$MP/2000), hld[,j])
}

HYB <- as.data.frame(matrix(nrow = nrow(NPI), ncol = ncol(NPI)))
colnames(HYB) <- colnames(NPI)
HYB[, 1:4] <- hld[, 1:4]

	HYB$EFG.O <- summary(efgo)$r.squared*hld$EFG.O.y + (1-summary(efgo)$r.squared)*hld$EFG.O.x
	HYB$EFG.D <- summary(efgd)$r.squared*hld$EFG.D.y + (1-summary(efgd)$r.squared)*hld$EFG.D.x
	HYB$FT.O <- summary(fto)$r.squared*hld$FT.O.y + (1-summary(fto)$r.squared)*hld$FT.O.x
	HYB$FT.D <- summary(ftd)$r.squared*hld$FT.D.y + (1-summary(ftd)$r.squared)*hld$FT.D.x
	HYB$TOV.O <- summary(tovo)$r.squared*hld$TOV.O.y + (1-summary(tovo)$r.squared)*hld$TOV.O.x
	HYB$TOV.D <- summary(tovd)$r.squared*hld$TOV.D.y + (1-summary(tovd)$r.squared)*hld$TOV.D.x
	HYB$REB.O <- summary(rebo)$r.squared*hld$REB.O.y + (1-summary(rebo)$r.squared)*hld$REB.O.x
	HYB$REB.D <- summary(rebd)$r.squared*hld$REB.D.y + (1-summary(rebd)$r.squared)*hld$REB.D.x
	HYB$MRG.O <- summary(mrgo)$r.squared*hld$MRG.O.y + (1-summary(mrgo)$r.squared)*hld$MRG.O.x
	HYB$MRG.D <- summary(mrgd)$r.squared*hld$MRG.D.y + (1-summary(mrgd)$r.squared)*hld$MRG.D.x
	HYB$EFG <- HYB$EFG.O + HYB$EFG.D
	HYB$FT <- HYB$FT.O + HYB$FT.D
	HYB$TOV <- HYB$TOV.O + HYB$TOV.D
	HYB$REB <- HYB$REB.O + HYB$REB.D
	HYB$NET <- HYB$MRG.O + HYB$MRG.D

for(j in c(4:12, 15:18)){
	HYB[,j] <- round(HYB[,j], 3)
}
for(j in c(13:14, 19)){
	HYB[,j] <- round(HYB[,j], 2)
}
HYB <- na.omit(HYB)


for(j in c(4:12, 15:18, 20:27, 30:33)){
	hld[,j] <- round(hld[,j], 3)
}
for(j in c(13:14, 19, 28:29, 34)){
	hld[,j] <- round(hld[,j], 2)
}
PM <- as.data.frame(matrix(nrow = nrow(NPI), ncol = ncol(NPI)))
colnames(PM) <- colnames(NPI)
PM[, 1:19] <- hld[, 1:19]
PM <- na.omit(PM)

BX <- as.data.frame(matrix(nrow = nrow(NPI), ncol = ncol(NPI)))
colnames(BX) <- colnames(NPI)
BX[, 1:19] <- hld[, c(1:4, 20:34)]
BX <- na.omit(BX)

write.csv(HYB, "HYB.OUT.csv", row.names=F)
write.csv(BX, "BPM.OUT.csv", row.names=F)
write.csv(PM, "NPI.OUT.csv", row.names=F)

























# Hybrid output
BPM <- read.csv("BPM.csv")
TST <- merge(BPM, PRIOR, by = c("Name", "ID", "Season"), all.x = T)
for(j in c(6,8,9,12,14,16,18,19,22,24)){
	TST[,j] <- -TST[,j]
}
for(j in 5:24){
	TST <- TST[order(TST[,j]), ]
	TST[,j] <- (1:nrow(TST))/nrow(TST)
}
	bx.wgt <- TST$MP/3500
	pm.wgt <- 1-bx.wgt
	TST[,5:14] <- TST[,5:14]*pm.wgt + TST[,15:24]*bx.wgt
	TST <- TST[1:14]

TST.pre <- TST
	TST.pre$Season <- TST.pre$Season+1
	colnames(TST.pre) <- c("Name", "ID", "Season", "MP.pre", 
			       "EFG.O.pre", "EFG.D.pre", "FT.O.pre", "FT.D.pre", "TOV.O.pre", "TOV.D.pre", "REB.O.pre", "REB.D.pre", "MRG.O.pre", "MRG.D.pre")
TST <- merge(TST, TST.pre, by = c("Name", "ID", "Season"), all.x = T)
TST[is.na(TST)] <- 0

	ct.wgt <- (TST$MP*3)/(TST$MP*3 + TST$MP.pre)
	pt.wgt <- 1-ct.wgt
	TST[, 5:14] <- TST[, 5:14]*ct.wgt + TST[, 16:25]*pt.wgt
	TST <- TST[1:14]

TST$EFG <- (TST$EFG.O + TST$EFG.D)/2
TST$FT <- (TST$FT.O + TST$FT.D)/2
TST$TOV <- (TST$TOV.O + TST$TOV.D)/2
TST$REB <- (TST$REB.O + TST$REB.D)/2	
TST$NET <- (TST$MRG.O + TST$MRG.D)/2
for(j in 15:19){
	TST <- TST[order(TST[,j]), ]
	TST[,j] <- (1:nrow(TST))/nrow(TST)
}

colnames(TST)[13:14] <- c("O", "D")
for(j in 5:19){
	TST[,j] <- round(TST[,j], 3)
}
TST <- merge(TST, plr, by = c("Name", "ID", "Season", "MP"))
TST <- TST[c(1, 20, 3:19)]

write.csv(TST, "HYB.OUT.csv", row.names=F)

#  NPI output
NPI <- read.csv("NPI.csv")[c(1:2,14,4:13)]
	NPI <- merge(NPI, plr, by = c("Name", "ID", "Season"))
	NPI <- NPI[c(1, 14, 3, 15, 4:13)]
	for(j in c(6, 8, 9, 12, 14)){
		NPI[,j] <- -NPI[,j]
	}
NPI$EFG <- NPI$EFG.O + NPI$EFG.D
NPI$FT <- NPI$FT.O + NPI$FT.D
NPI$TOV <- NPI$TOV.O + NPI$TOV.D
NPI$REB <- NPI$REB.O + NPI$REB.D
NPI$NET <-  NPI$MRG.O + NPI$MRG.D

for(j in 5:19){
	NPI <- NPI[order(NPI[,j]), ]
	NPI[,j] <- round((1:nrow(NPI))/nrow(NPI), 2)
}

write.csv(NPI, "NPI.OUT.csv", row.names=F)


#BPM output
	PRIOR <- merge(PRIOR, plr, by = c("Name", "ID", "Season"))
	PRIOR <- PRIOR[c(1, 14, 3, 15, 4:13)]
	for(j in c(6, 8, 9, 12, 14)){
		PRIOR[,j] <- -PRIOR[,j]
	}
PRIOR$EFG <- PRIOR$efgo + PRIOR$efgd
PRIOR$FT <- PRIOR$fto + PRIOR$ftd
PRIOR$TOV <- PRIOR$tovo + PRIOR$tovd
PRIOR$REB <- PRIOR$rebo + PRIOR$rebd
PRIOR$NET <-  PRIOR$mrgo + PRIOR$mrgd

colnames(PRIOR) <- colnames(NPI)

f#or(j in 5:19){
#	PRIOR <- PRIOR[order(PRIOR[,j]), ]
#	PRIOR[,j] <- round((1:nrow(PRIOR))/nrow(PRIOR), 2)
#}

write.csv(PRIOR, "BPM.OUT.csv", row.names=F)












#########  4FBPM
BPM$TOV.O <- -BPM$TOV.O
BPM$TOV.D <- -BPM$TOV.D

BPM <- BPM[order(BPM$Season), ]
strt <- nrow(subset(BPM, Season == 2001))
for(i in (strt+1):nrow(BPM)){
	for(j in 3:ncol(BPM)){
		reg <- BPM[i,j]*(BPM$MP[i]/max(BPM$MP))
		BPM[i,j] <- (1-reg)*ifelse(length(BPM[,j][BPM$Season == BPM$Season[i] - 1]) < 1, BPM[,3][BPM$Season == s], -wt.sd(BPM[,3]) + 
			reg*BPM[,3][BPM$Season == s]				   





TST <- na.omit(merge(tst, subset(PRIOR, Season == 2015), all.x = T)[-2])
	for(j in 4:ncol(TST)){
		TST <- TST[order(TST[,j]), ]
		TST[,j] <- 1:nrow(TST)
	}
OUTPUT <- data.frame("Name"=TST$Name, "MP"=TST$MP,
		      "EFG.O"=TST$EFG.O*(TST$MP/3000) + TST$efgo*(1-(TST$MP/3000)), 
		      "FT.O"=TST$FT.O*(TST$MP/3000) + TST$fto*(1-(TST$MP/3000)), 
		      "TOV.O"=-(TST$TOV.O*(TST$MP/3000) +TST$tovo*(1-(TST$MP/3000))), 
		      "REB.O"=TST$REB.O*(TST$MP/3000) + TST$rebo*(1-(TST$MP/3000)), 		      
		      "EFG.D"=-(TST$EFG.D*(TST$MP/3000) + TST$efgd*(1-(TST$MP/3000))), 
		      "FT.D"=-(TST$FT.D*(TST$MP/3000) + TST$ftd*(1-(TST$MP/3000))), 
		      "TOV.D"=TST$TOV.D*(TST$MP/3000) + TST$tovd*(1-(TST$MP/3000)), 
		      "REB.D"=-(TST$REB.D*(TST$MP/3000) + TST$rebd*(1-(TST$MP/3000))),
		      "O"=TST$MRG.O*(TST$MP/3000) + TST$mrgo*(1-(TST$MP/3000)), 
		      "D"=-(TST$MRG.D*(TST$MP/3000) + TST$mrgd*(1-(TST$MP/3000))))
OUTPUT$NET <- (OUTPUT$O + OUTPUT$D)/2
#for(j in 3:ncol(OUTPUT)){
#	sit <- (max(OUTPUT$MP)+1) - OUTPUT$MP
#	OUTPUT[,j] <- scale(OUTPUT[,j])
#	OUTPUT[,j] <- OUTPUT[,j]*(OUTPUT$MP/mx) + -1.5*(1 - OUTPUT$MP/mx)
#}

for(j in 3:ncol(OUTPUT)){
	OUTPUT <- OUTPUT[order(OUTPUT[,j]), ]
	OUTPUT[,j] <- 1:nrow(OUTPUT)
	OUTPUT[,j] <- round(OUTPUT[,j]/nrow(OUTPUT), 3)
}







TST <- data.frame(BPM, 
		  "EFGO.pre"=NA, "EFGD.pre"=NA, "FTO.pre"=NA, "FTD.pre"=NA, "TOVO.pre"=NA, "TOVD.pre"=NA, "REBO.pre"=NA, "REBD.pre"=NA, "MRGO.pre"=NA, "MRGD.pre"=NA,
		  "EFGO.pst"=NA, "EFGD.pst"=NA, "FTO.pst"=NA, "FTD.pst"=NA, "TOVO.pst"=NA, "TOVD.pst"=NA, "REBO.pst"=NA, "REBD.pst"=NA, "MRGO.pst"=NA, "MRGD.pst"=NA)
for(i in 1:nrow(TST)){
	TST[i,15:24] <- ifelse(length(TST$ID[TST$ID == TST$ID[i] & TST$Season == TST$Season[i]-1]) == 1, 
				  as.numeric(TST[TST$ID == TST$ID[i] & TST$Season == TST$Season[i]-1, 4:13]), rep(NA, 10))
	TST[i,25:34] <- ifelse(length(TST$ID[TST$ID == TST$ID[i] & TST$Season == TST$Season[i]+1]) == 1, 
				  as.numeric(TST[TST$ID == TST$ID[i] & TST$Season == TST$Season[i]+1, 4:13]), rep(NA, 10))
}
TST <- subset(TST, Season >= 2002)
	for(j in 15:24){
		TST[is.na(TST[,j]), j] <- wt.mean(TST[is.na(TST[,j]), j-11], TST[is.na(TST[,j]), 3])
	}	


TST <- merge(TST, PRIOR, by = c("Name", "ID", "Season"), all.x = T)
for(j in 5:44){
	TST[,j] <- scale(TST[,j])
}
	for(j in 5:14){
		m1 <- lm(TST[,j+20] ~ TST[,j] + TST[,j+30], weights = MP, data = TST)
		pm.wgt <- as.numeric(coef(m1)[2]/(coef(m1)[2] + coef(m1)[3]))
		box.wgt <- as.numeric(1 - pm.wgt)
		for(i in 1:nrow(TST)){
			TST[i,j] <- TST[i,j]*pm.wgt+ TST[i,j+30]*box.wgt
		}
	}

for(i in 1:nrow(TST)){
	TST[i,15:24] <- ifelse(length(TST$ID[TST$ID == TST$ID[i] & TST$Season == TST$Season[i]-1]) == 1, 
				  as.numeric(TST[TST$ID == TST$ID[i] & TST$Season == TST$Season[i]-1, 4:13]), rep(NA, 10))
	TST[i,25:34] <- ifelse(length(TST$ID[TST$ID == TST$ID[i] & TST$Season == TST$Season[i]+1]) == 1, 
				  as.numeric(TST[TST$ID == TST$ID[i] & TST$Season == TST$Season[i]+1, 4:13]), rep(NA, 10))
}
for(j in 5:44){
	TST[,j] <- scale(TST[,j])
}

	for(j in 5:14){
		m1 <- lm(TST[,j+20] ~ TST[,j] + TST[,j+10], weights = MP, data = TST)
		crnt.wgt <- as.numeric(coef(m1)[2]/(coef(m1)[2] + coef(m1)[3]))
		past.wgt <- as.numeric(1 - crnt.wgt)
		for(i in 1:nrow(TST)){
			TST[i,j] <- TST[i,j]*crnt.wgt+ TST[i,j+10]*past.wgt
		}
	}

