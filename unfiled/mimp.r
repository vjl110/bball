rm(list=ls())
#setwd("C:/Users/Layne/Dropbox/Archive/BBall/Draft proj")
setwd("C:/Users/SCAN Project/Dropbox/Archive/BBall/Draft proj")


library(mi)

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
SCHOOL <- SCHOOL[-c(1, 4:7, 10:13, 15)]
SCHOOL$SRS <- SCHOOL$SRS - SCHOOL$SOS
colnames(SCHOOL) <-c("College", "Season", "MOV", "SOS", "Coach") 
ncaa <- merge(ncaa, SCHOOL, by=c("College", "Season"), all.x=T)
ncaa$SOS[is.na(ncaa$SOS)] <- -15
ncaa$MOV[is.na(ncaa$MOV)] <- 0

#NBA data
nba <- read.csv("BBRtot.csv")
nba <- subset(nba, Tm != "TOT")
nba.sum <- nba[-c(3:6, 8)]
nba.mean <- nba[-c(4:5, 7:24)]
nba.sum <- aggregate( . ~ Name + Season, data = nba.sum, sum)
nba.mean <- aggregate( . ~ Name + Season, data = nba.mean, mean)
nba <- merge(nba.mean, nba.sum, by=c('Name','Season'))

nba <- nba[order(nba$Season), ]
nba <- subset(nba, !duplicated(Name))

for(i in 7:ncol(nba)){
	nba[,i] <- round((nba[,i]/nba[,6])*36, 1)
}

nba <- nba[-c(3, 5, 14)]

colnames(nba) <- c("Name", "Season.nba", "Pos.nba", "MP.nba", "X2P.nba", "X2PA.nba", "X3P.nba", "X3PA.nba", "FT.nba", "FTA.nba", "ORB.nba", "TRB.nba", "AST.nba", "STL.nba", "BLK.nba", "TOV.nba", "PF.nba", "PTS.nba")

#merging

data <- merge(ncaa, nba, all.x=T)

# Add physical variables
bio <- read.csv("BBRbio.csv", strip.white = T)
data <- merge(data, bio, by="Name", all.x=T)

phyz <- read.csv("phyz.csv", strip.white = T)
phyz <- phyz[-2]
phyz <- phyz[order(phyz$Wingspan),]
phyz <- phyz[order(phyz$NS_vert),]
phyz <- subset(phyz, !duplicated(Name))

data <- merge(data, phyz, by="Name", all.x=T)
data$Height <- ifelse(!is.na(data$inches), data$inches, data$Height)
data$Weight <- ifelse(!is.na(data$lbs), data$lbs, data$Weight)
data <- data[-c(ncol(data), (ncol(data) - 1))]

data$Age <- (data$Season-1900)*365 - data$DOB
data$Age.nba <- (data$Season.nba-1900)*365 - data$DOB



##########
########  IMPUTE  ###
#######
data <- subset(data, !is.na(MP))


data <- subset(data, Season > 2001)

data <- subset(data, !is.na(PTS.nba))

data <- subset(data, !is.na(STL))

data <- subset(data, !is.na(Coach))

inf <- mi.info(data)
# run the imputation with data transformation  
dat.transformed <- mi.preprocess(data, inf)
#IMP <- mi(dat.transformed, n.iter=6, check.coef.convergence=TRUE, 
#  add.noise=noise.control(post.run.iter=6))

IMP <- mi(dat.transformed, n.iter=6, add.noise=FALSE)

## this is the suggested (defautl) way of running mi
IMP <- mi(data, info=inf, max.minutes=10) ## NOT RUN




