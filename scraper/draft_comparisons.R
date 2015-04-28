setwd("~/GitHub/bball/data/draftmod")


d <- read.csv("OUTPUT.DRAFT.csv")

slot.avg <- c()
slot.sd <- c()
slot.15 <- c()
for(i in 1:30){
	slot.avg[i] <- mean(d$EWP[d$Season < 2015 & d$Pick == i], na.rm=T)
	slot.sd[i] <- sd(d$EWP[d$Season < 2015 & d$Pick == i], na.rm=T)	
	slot.15[i] <- ifelse(length(d$EWP[d$Season == 2015 & d$Pick == i]) == 1,
			     d$EWP[d$Season == 2015 & d$Pick == i], NA)		
}


png("draftcomp.png", height = 500, width = 500)
plot(NULL, xlim = c(1, 30), ylim = c(0, 15), xlab = "Pick", ylab = "Expected Wins Peak", main = "2015 vs. Average Draft", font = 2, font.lab = 2)
library(msir)
low <- loess.sd(d$Pick[!is.na(d$Pick) & d$Pick <= 30 & !is.na(d$EWP) & d$Season < 2015], d$EWP[!is.na(d$Pick) & d$Pick <= 30 & !is.na(d$EWP) & d$Season < 2015])
	lines(low$x, low$upper, col = "light grey")
	lines(low$x, low$lower, col = "light grey")
		polygon(c(low$x, rev(low$x)), c(low$upper, rev(low$lower)), col = "light grey", border = NA)	
	lines(low$x, low$y)
points(1:30, slot.15, pch = 16, col = "red")
points(1:30, slot.avg, pch = 16)	
	
low15 <- loess.sd(d$Pick[!is.na(d$Pick) & d$Pick <= 30 & !is.na(d$EWP) & d$Season == 2015], d$EWP[!is.na(d$Pick) & d$Pick <= 30 & !is.na(d$EWP) & d$Season == 2015])
	lines(low15$x, low15$y, col = "red", lwd = 2.5)
dev.off()



corz <- c()
for(i in 1990:2015){
	corz[i-1989] <- cor(d$EWP[d$Season ==i & !is.na(d$EWP) & !is.na(d$Pick)], d$Pick[d$Season== i & !is.na(d$EWP) & !is.na(d$Pick)])
}


corz <- c()
for(i in 1990:2015){
	corz[i-1989] <- cor(d$HUM[d$Season ==i & !is.na(d$HUM) & !is.na(d$Pick)], d$Pick[d$Season== i & !is.na(d$HUM) & !is.na(d$Pick)])
}
