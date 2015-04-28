library(shiny)
library(wordcloud)

dat <- read.csv("data/prog_dat.csv")
shinyServer(function(input, output) {

  output$plot1 <- renderPlot({

      validate(
       	   need(input$plr != "", "Please select a player")
      )    

	set <- subset(dat, Name == input$plr & SRS >= input$srs[1] & SRS <= input$srs[2])
            validate(
       	   need(nrow(set) > 0, "No data")
      )    

      	set <- set[c(1:3, as.numeric(input$stat))]
	set[,4] <- as.numeric(set[,4])
      min.x <- min(set$Rk, na.rm=T)
      max.x <- max(set$Rk, na.rm=T)
      min.y <- min(dat[,as.numeric(input$stat)], na.rm=T)
      max.y <- max(dat[,as.numeric(input$stat)], na.rm=T)

#plot(NULL, xlim = c(min.x-1, max.x+1), ylim = c(min.y-.1*max.y, max.y), xlab = "Game", ylab = paste(colnames(set)[4]), main = paste(input$plr), font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5, pin = c(5, 5))
	textplot(set$Rk, set[,4], words = set$Opponent, xlim = c(min.x-1, max.x+1), ylim = c(min.y-.1*max.y, max.y), xlab = "Game", ylab = paste(colnames(set)[4]), main = paste(input$plr), font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5, pin = c(5, 5))
	abline(h = mean(dat[,as.numeric(input$stat)], na.rm=T), lty = 2)
	abline(h = mean(set[,4], na.rm = T), lty = 1, col = "salmon")
		lws <- loess(set[,4] ~ Rk, data = set, span = as.numeric(input$spn))
		lines(1:max(set$Rk, na.rm=T), predict(lws, 1:max(set$Rk, na.rm=T)), col = "red", lwd = 2)
  }, height = 750, width = 750)
}) 





