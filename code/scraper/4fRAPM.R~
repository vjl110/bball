setwd("~/GitHub/bball")
library(glmnet) #load glmnet package
library(SDMTools) #load glmnet package


NPI <- read.csv("NPI.csv")
HOME.adv <- read.csv("HOME.adv.csv")
###
###
HOME.adv <- data.frame("Season" = 2001:2015, 
		       "EFG.df"=NA,  "FT.df"=NA,  "TOV.df"=NA,  "REB.df"=NA, "MRG.df" = NA, "EFG.od"=NA,  "FT.od"=NA,  "TOV.od"=NA,  "REB.od"=NA, "MRG.od" = NA) 
DMP <- list()
for(s in 2001:2015){
	#####  Basic pre-pred that applies to all 4 factors
	lnp.yr <- subset(read.csv("LUP_RPM.csv", stringsAsFactors=FALSE), (Season == s | Season == s-1 | Season == s+1) & !is.na(EFG.df) & !is.na(FT.df) & !is.na(TOV.df) & !is.na(REB.df) & POSS > 0)
	plr.yr <- subset(read.csv("PLR_RPM.csv", stringsAsFactors=FALSE), Season == s | Season == s-1 | Season == s+1)
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
	lambda <- cv.glmnet(X, dv_df, weights=wgt) #find the lambda values. these determine how far towards 0 the coefficients are shrunk
	lambda.1se <- lambda$lambda.1se #store the lambda value that gives the smallest error in an object called lambda.min
	ridge <- glmnet(X, dv_df, family=c("gaussian"), wgt, alpha=0, lambda=lambda.1se) #
		op <- coef(ridge ,s=lambda.1se) #extract the coefficient for each of the independent variables (players) for the lambda with the minimum error 
			op <- as.data.frame(as.matrix(op))
				op$ID <- c("INT", colnames(data))
					colnames(op) <- c("df", "ID")
						row.names(op) <- 1:nrow(op)
						HOME.adv[s-2000, i+1] <- op[2,1]
							op_df <- na.omit(merge(op, plr.yr, by = "ID", all.x=T))

	lambda <- cv.glmnet(X2, dv_od, weights=wgt) #find the lambda values. these determine how far towards 0 the coefficients are shrunk
	lambda.1se <- lambda$lambda.1se #store the lambda value that gives the smallest error in an object called lambda.min
	ridge <- glmnet(X2, dv_od, family=c("gaussian"), wgt, alpha=0, lambda=lambda.1se) #
		op <- coef(ridge ,s=lambda.1se) #extract the coefficient for each of the independent variables (players) for the lambda with the minimum error 
			op <- as.data.frame(as.matrix(op))
				op$ID <- c("INT", colnames(data))
					colnames(op) <- c("od", "ID")
						row.names(op) <- 1:nrow(op)
						HOME.adv[s-2000, i+6] <- op[2,1]				
							op_od <- na.omit(merge(op, plr.yr, by = "ID", all.x=T))
			
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
		plr.set <- subset(read.csv("PLR_RPM.csv", stringsAsFactors=FALSE), Season == s | Season == s-1 | Season == s+1)[c(1, 9, 61)]
		plr.set <- aggregate( . ~ Name + ID, data = plr.set, sum)
	output <- na.omit(merge(plr.set, output, by = c("Name", "ID"), all.x = T))
	output <- subset(output, !duplicated(Name))
	output$Season <- s
	DMP[[s - 2000]] <- output
		rm(output)
}
NPI <- do.call("rbind", DMP)
write.csv(NPI, "NPI.csv", row.names = F)
write.csv(HOME.adv, "HOME.adv.csv", row.names = F)

#########  4FBPM

NPI <- read.csv("NPI.csv")[-3]
	plr <- read.csv("PLR_RPM.csv", stringsAsFactors=FALSE)
	plr <- plr[c(61, 1, 2, 4, 6, 7, 9, 11, 13:14, 16:17, 20:21, 30, 32:33, 35:38, 42:46, 53, 57)]
	plr[is.na(plr)] <- 0
	plr$Pos[plr$Pos == "PG"] <- 1
	plr$Pos[plr$Pos == "SG"] <- 2
	plr$Pos[plr$Pos == "SF"] <- 3
	plr$Pos[plr$Pos == "PF"] <- 4
	plr$Pos[plr$Pos == "C"] <- 5
	plr$Pos <- as.numeric(plr$Pos)	
	for(i in 1:nrow(plr)){
		plr$mp.tot[i] <- sum(plr$MP[plr$Name == plr$Name[i] & plr$Season == plr$Season[i]], na.rm=T)
	}	
	plr$mp.tot <- plr$MP/plr$mp.tot
	for(j in c(5, 16:28)){
		plr[,j] <- plr[,j]*plr$mp.tot
	}
	plr <- aggregate( . ~ Name + ID + Season + Age, data = plr, sum)[-ncol(plr)]

NPI <- merge(plr, NPI, by = c("Name", "ID", "Season"), all.x = T) # are there multi in season?  If so agg...

for(j in 8:15){
	NPI[,j] <- (NPI[,j]/NPI$MP)*40
}
NPI <- subset(NPI, Season >= 2002)
	prev <- read.csv("NPI.csv")[-3]
	prevmp <- plr[c(1:3, 7)] 
	prev$Season <- prev$Season + 1
	prevmp$Season <- prevmp$Season + 1
	colnames(prevmp)[4] <- "preMP"	
	NPI <- merge(NPI, prev, by = c("Name", "ID", "Season"), all.x = T)
	NPI <- merge(NPI, prevmp, by = c("Name", "ID", "Season"), all.x = T)

for(j in 39:48){
	NPI[,j][is.na(NPI[,j])] <- wt.mean(NPI[,j-10][is.na(NPI[,j])], NPI$MP[is.na(NPI[,j])]) 
}

rm(plr)

#### EFG  ###
	#O	
	efgo <- lm(EFG.O.x ~ EFG.O.y +
		   	I(per0.3*FGA) + I(per3.10*FGA) + I(per10.16*FGA) + I(per16.23*FGA) + I((per3P-perCRNR)*FGA) + I((perCRNR)*FGA) + 
			I((X2P+X3P*1.5)/FGA) + I(Asd.2*FGA) + AST. + I(MP/G) + Pos + Age + I(Age^2)
	  	, data = NPI, weights = MP)
	summary(efgo)
	#D
	efgd <- lm(EFG.D.x ~  EFG.D.y +
		   	BLK.*PF + STL. + I(MP/G) + Pos + Age + I(Age^2) + ORB.		   
		     , data = NPI, weights = MP)	
	summary(efgd)
#### FT  ###
	#O	
	fto <- lm(FT.O.x ~  FT.O.y +
		  FT + TOV. + ORB. + FGA + I(MP/G) + Pos + Age + I(Age^2)
		  , data = NPI, weights = MP)       	
	summary(fto)
	#D
	ftd <- lm(FT.D.x ~ FT.D.y +  
		  BLK. + PF + Pos
		  , data = NPI, weights = MP)	
	summary(ftd)

#### TOV  ###
	#O	
	tovo <- lm(TOV.O.x ~  TOV.O.y +
		   	AST. + TOV. +
			I(per0.3*FGA) + I(per3.10*FGA) + I(per10.16*FGA) + I(per16.23*FGA) + I((per3P-perCRNR)*FGA) + I((perCRNR)*FGA) + 
			Pos + Age	   
		     , data = NPI, weights = MP)	
	summary(tovo)
	#D
	tovd <- lm(TOV.D.x ~  TOV.D.y +
		   STL. + BLK. + PF + DRB.
		   , data = NPI, weights = MP)	
	summary(tovd)
#### REB  ###
	#O	
	rebo <- lm(REB.O.x ~  REB.O.y +
		   	ORB. + Pos +
			I(per0.3*FGA) + I((per3P-perCRNR)*FGA) + I((perCRNR)*FGA) 
		 , data = NPI, weights = MP)	
	summary(rebo)
	#D
	rebd <- lm(REB.D.x ~  REB.D.y +
		   	DRB. + STL. + PF + I(MP/G) + Pos + Age + BLK.
		   , data = NPI, weights = MP)		
	summary(rebd)
#########
NPI$efgo <- predict(efgo, newdat = NPI) 
NPI$efgd <- predict(efgd, newdat = NPI) 
NPI$fto <- predict(fto, newdat = NPI) 
NPI$ftd <- predict(ftd, newdat = NPI) 
NPI$tovo <- predict(tovo, newdat = NPI) 
NPI$tovd <- predict(tovd, newdat = NPI) 
NPI$rebo <- predict(rebo, newdat = NPI) 
NPI$rebd <- predict(rebd, newdat = NPI) 
#### REB  ###
	#O	
	mrgo <- lm(MRG.O.x ~ MRG.O.y + efgo + fto + tovo + rebo
		 , data = NPI, weights = MP)	
	summary(mrgo)
	#D
	mrgd <- lm(MRG.D.x ~  MRG.D.y + efgd + ftd + tovd + rebd
		   , data = NPI, weights = MP)		
	summary(mrgd)
#########
NPI$mrgo <- predict(mrgo, newdat = NPI) 
NPI$mrgd <- predict(mrgd, newdat = NPI) 

PRIOR <- na.omit(NPI[c(1:3, 49:ncol(NPI))])

####
####	Statistical Prior

DMP <- list()
for(s in 2002:2015){
	#####  Basic pre-pred that applies to all 4 factors
	lnp.yr <- subset(read.csv("LUP_RPM.csv", stringsAsFactors=FALSE), Season == s & !is.na(EFG.df) & !is.na(FT.df) & !is.na(TOV.df) & !is.na(REB.df) & POSS > 0)
		lnp.yr <- lnp.yr[order(lnp.yr$HOME, decreasing = T), ]
		hm.brk <- nrow(subset(lnp.yr, HOME == 1))
	plr.yr <- subset(read.csv("PLR_RPM.csv", stringsAsFactors=FALSE), Season == s)
		plr.yr <- plr.yr[,c(1:2, ncol(plr.yr))]
			plr.yr <- subset(plr.yr, !duplicated(ID))
				plr.yr <- merge(plr.yr, PRIOR, by = c("Name", "ID", "Season"), all.x = T)
					for(j in 5:14){
						plr.yr[,j][is.na(plr.yr[,j])] <- mean(plr.yr[,j], na.rm=T)
					}
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
	OFF_Os <- list(plr.yr[c(2, 5)], plr.yr[c(2, 7)], plr.yr[c(2, 9)], plr.yr[c(2, 11)], plr.yr[c(2,13)])
	OFF_Ds <- list(plr.yr[c(2, 6)], plr.yr[c(2, 8)], plr.yr[c(2, 10)], plr.yr[c(2, 12)], plr.yr[c(2, 14)])	
	WGTs <- list(lnp.yr$FGA, lnp.yr$FGA, lnp.yr$POSS.to, lnp.yr$RB.chance, lnp.yr$POSS)


	coef_hld <- list()
	for(i in 1:length(DV_DFs)){
		os_df <- lnp.yr[24:ncol(lnp.yr)]
		os_od <- lnp.yr[24:ncol(lnp.yr)]
		for(j in 1:ncol(os_df)){
			os_df[,j][os_df[,j] == 1] <- OFF_Os[[i]][,2][OFF_Os[[i]][,1] == colnames(os_df[j])]
			os_df[,j][os_df[,j] == -1] <- OFF_Ds[[i]][,2][OFF_Ds[[i]][,1] == colnames(os_df[j])]		
			os_od[,j][os_od[,j] == 1] <- OFF_Os[[i]][,2][OFF_Os[[i]][,1] == colnames(os_df[j])]
			os_od[,j][os_od[,j] == -1] <- -OFF_Ds[[i]][,2][OFF_Ds[[i]][,1] == colnames(os_df[j])]		
		}
	os_df <- rowSums(os_df, na.rm=T)
		os_df[1:hm.brk] <- os_df[1:hm.brk] + as.numeric(colMeans(HOME.adv)[i+1])
	os_od <- rowSums(os_od, na.rm=T)
		os_od[1:hm.brk] <- os_od[1:hm.brk] + as.numeric(colMeans(HOME.adv)[i+6])
	
		mndf <- wt.mean(DV_DFs[[i]], WGTs[[i]])
		sddf  <- wt.sd(DV_DFs[[i]], WGTs[[i]])
			os_df <- mndf + sddf*scale(os_df)
		mnod <- wt.mean(DV_ODs[[i]], WGTs[[i]])
		sdod  <- wt.sd(DV_ODs[[i]], WGTs[[i]])
			os_od <- mnod + sdod*scale(os_od)		

	dv_df <- DV_DFs[[i]]
	dv_od <- DV_ODs[[i]]
	wgt <- WGTs[[i]]
	lambda <- cv.glmnet(X, dv_df, offset = os_df, weights=wgt, alpha=0) #find the lambda values. these determine how far towards 0 the coefficients are shrunk
	lambda.1se <- lambda$lambda.1se #store the lambda value that gives the smallest error in an object called lambda.min
	ridge <- glmnet(X, dv_df, family=c("gaussian"), offset = os_df, wgt, alpha=0, lambda=lambda.1se) #
		op <- coef(ridge ,s=lambda.1se) #extract the coefficient for each of the independent variables (players) for the lambda with the minimum error 
			op <- as.data.frame(as.matrix(op))
				op$ID <- c("INT", colnames(data))
					colnames(op) <- c("df", "ID")
						row.names(op) <- 1:nrow(op)
							op_df <- na.omit(merge(op, plr.yr[1:3], by = "ID", all.x=T))

	lambda <- cv.glmnet(X2, dv_od, offset = os_od, weights=wgt, alpha=0) #find the lambda values. these determine how far towards 0 the coefficients are shrunk
	lambda.1se <- lambda$lambda.1se #store the lambda value that gives the smallest error in an object called lambda.min
	ridge <- glmnet(X2, dv_od, family=c("gaussian"), offset = os_od, wgt, alpha=0, lambda=lambda.1se) #
		op <- coef(ridge ,s=lambda.1se) #extract the coefficient for each of the independent variables (players) for the lambda with the minimum error 
			op <- as.data.frame(as.matrix(op))
				op$ID <- c("INT", colnames(data))
					colnames(op) <- c("od", "ID")
						row.names(op) <- 1:nrow(op)
							op_od <- na.omit(merge(op, plr.yr[1:3], by = "ID", all.x=T))
			
	op <- merge(op_df[-4], op_od[-4], by = c( "ID", "Name"))
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
	DMP[[s - 2001]] <- output
		rm(output)
}

BPM <- do.call("rbind", DMP)
write.csv(BPM, "BPM.csv", row.names = F)

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

















	
DMP <- list()
for(s in 2001:2015){
	lnp.yr <- subset(read.csv("LUP_RPM.csv", stringsAsFactors=FALSE), Season == s)
	plr.yr <- subset(read.csv("PLR_RPM.csv", stringsAsFactors=FALSE), Season == s)
		plr.yr <- plr.yr[,c(1:2, 4, ncol(plr.yr)-2)]
			plr.yr <- subset(plr.yr, !duplicated(ID))
	lnp.yr[plr.yr$ID] <- 0
	row.names(lnp.yr) <- 1:nrow(lnp.yr)
	for(j in 34:ncol(lnp.yr)){
		ego <- grep(colnames(lnp.yr)[j], lnp.yr$LINEUP)
		alt <- grep(colnames(lnp.yr)[j], lnp.yr$CP_LINEUP)
		lnp.yr[row.names(lnp.yr) %in% ego, j] <- 1
		lnp.yr[row.names(lnp.yr) %in% alt, j] <- -1
	}
	data <- data.matrix(lnp.yr[c(6,34:ncol(lnp.yr))]) #turn the data frame (which is now just 1s, -1s, and 0s) into a matrix
	X <- sparse.model.matrix(~data[,1]-1)
	for (i in 2:ncol(data)) {
		    coluna <- sparse.model.matrix(~data[,i]-1)
 			 X <- cBind(X, coluna)
	}
	dv_df <- lnp.yr$EFG.dif
	dv_od <- lnp.yr$efg.OD
	wgt <- lnp.yr$FGA_O
	
	NPI.hld <- read.csv("NPI.csv", stringsAsFactors=FALSE)
		NPI.hld$Season <- NPI.hld$Season + 1
	plr.yr <- merge(plr.yr, NPI.hld, by = c("ID", "Name.full", "Season"), all.x = T)
		rm(NPI.hld)
	row.names(plr.yr) <- 1:nrow(plr.yr)
	for(i in as.numeric(row.names(plr.yr[is.na(plr.yr$df),]))){
		plr.yr$df[i] <- crv.age$df[crv.age$Age == plr.yr$Age[i]]
	}
	for(i in as.numeric(row.names(plr.yr[is.na(plr.yr$od),]))){
		plr.yr$od[i] <- crv.age$od[crv.age$Age == plr.yr$Age[i]]
	}

	os_df <- lnp.yr[34:ncol(lnp.yr)]
	os_od <- lnp.yr[34:ncol(lnp.yr)]
	
	for(j in 1:ncol(os_df)){
		os_df[,j] <- os_df[,j]*plr.yr$df[j]
		os_od[,j] <- os_od[,j]*plr.yr$od[j]		
	}
	os_df <- rowSums(os_df, na.rm=T)
	os_od <- rowSums(os_od, na.rm=T)

	lambda <- cv.glmnet(X, dv_df, weights=wgt, offset = os_df, nfolds=5) #find the lambda values. these determine how far towards 0 the coefficients are shrunk
	lambda.min <- lambda$lambda.min #store the lambda value that gives the smallest error in an object called lambda.min
	ridge <- glmnet(X, dv_df, family=c("gaussian"), wgt, offset = os_df, alpha=0, lambda=lambda.min) #
		op <- coef(ridge ,s=lambda.min) #extract the coefficient for each of the independent variables (players) for the lambda with the minimum error 
			op <- as.data.frame(as.matrix(op))
				op$ID <- c("INT", colnames(data))
					colnames(op) <- c("df", "ID")
						row.names(op) <- 1:nrow(op)
							op_df <- na.omit(merge(op, plr.yr[1:3], by = "ID", all.x=T))
	lambda <- cv.glmnet(X, dv_od, weights=wgt, offset = os_od, nfolds=5) #find the lambda values. these determine how far towards 0 the coefficients are shrunk
	lambda.min <- lambda$lambda.min #store the lambda value that gives the smallest error in an object called lambda.min
	ridge <- glmnet(X, dv_od, family=c("gaussian"), wgt, offset = os_od, alpha=0, lambda=lambda.min) #
		op <- coef(ridge ,s=lambda.min) #extract the coefficient for each of the independent variables (players) for the lambda with the minimum error 
			op <- as.data.frame(as.matrix(op))
				op$ID <- c("INT", colnames(data))
					colnames(op) <- c("od", "ID")
						row.names(op) <- 1:nrow(op)
							op_od <- na.omit(merge(op, plr.yr[1:3], by = "ID", all.x=T))
	op <- merge(op_df, op_od, by = c( "ID", "Season", "Name.full"))
	DMP[[s - 2000]] <- op
}
ROLO <- do.call("rbind", DMP)

write.csv(ROLO, "ROLO.csv", row.names = F)







