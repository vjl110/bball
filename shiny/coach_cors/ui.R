library(shiny)

dat <- read.csv("data/corset.csv")
net <- read.csv("data/coach_net.csv")

shinyUI(pageWithSidebar(
  headerPanel('NCAA-to-NBA Correlations by Coach'),
  sidebarPanel(
      		selectInput("stat", "Statistic:", 
        		list("2PA"=22, "3PA"=23, "FTA"=24, "TRB"=25, "AST"=26, "STL"=27, "BLK"=28, "TOV"=29, "PF"=30, "PTS"=31, "2%"=32, "3%"=33, "FTR"=34, "ATO"=35), selected = ""),	       
		selectizeInput("cch", 
       	 		label = "Coach",
        		choices = c("", paste(unique(as.character(net$ego)))),
        		selected = "", options = list(maxOptions = 5000)),
    		sliderInput("pos", "Position:",
                	min = 1, max = 5, step = .5, value = c(1, 5)), 
	       width = 2),		
			
  mainPanel(
	h5("Coose the statistic you are interested in. Select the coach you want to highlight. Filter query to the positions you are interested in."),
	p("The resulting plot shows the relationship between NCAA and NBA production of that statistic.  Players under the selected coach are named and indicated with a red dot.  Pink dots indicate players for coaches one degree removed from the focal coach in a network based on assistantships.  See ",
	    a("here", href = "https://laynevashro.shinyapps.io/coach_nets/"), "for my coaching network tool to see who these coaches are."),
	p("Use the blue line to identify whether a given player performed better or worse in the NBA than his NCAA numbers predict.  If the point falls below the blue line, he did not collect statistic X as well in the NBA as an average player with his NCAA production.  If many players for a given coach and his relatives fall above/below the line, that may signal system effects worth noting for projecting current prospects."),	    
    plotOutput('plot1')
  )
))










