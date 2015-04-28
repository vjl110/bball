library(shiny)

dat <- read.csv("data/prog_dat.csv")

shinyUI(pageWithSidebar(
  headerPanel('2015 NBA Draft Prospects Game-by-Game'),
  sidebarPanel(
      		selectInput("stat", "Statistic:", 
        		list("VAL"= 32, "MP"=8, "PTS"=29, "FGA"=10, "FG%"=11, "FTA"=19, "ORB"=21, "DRB"=22, "AST"=24, "TOV"=27, "STL"=25, "BLK"=26, "PF"=28), selected = ""),	       
		selectizeInput("plr", 
       	 		label = "Player",
        		choices = c("", paste(unique(as.character(dat$Name)))),
        		selected = "", options = list(maxOptions = 5000)),
    		sliderInput("spn", "Smoothing:",
                	min = .25, max = 1, step = 0.05, value = 1), 
    		sliderInput("srs", "Opponent SRS:",
                	min = -35, max = 35, step = 1, value = c(-35, 35)), 
	       width = 2),		
			
  mainPanel(    
    plotOutput('plot1')
  )
))










