library(shiny)
ss <- read.csv("data/simset.nba.csv")

# Define UI for slider demo application
shinyUI(fluidPage(
  
  #  Application title
  titlePanel("NBA Player-Season Comparisons"),
 
	  p("Follow me on Twitter for updates", 
    	      a("@VJL_bball", href = "https://twitter.com/VJL_bball")),	    
	  a("Read the FAQ for a brief description of how best to use this tool", 
          href = "http://laynevashro.com/basketball/comps_nbaFAQ.html"),
	  br(),  
 	  br(),   

    # Create a new Row in the UI for selectInputs 
  
  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(	 
      # Name selector
      selectizeInput("nam", 
        label = "Choose Player",
        choices = c("", paste(unique(as.character(ss$Name)))),
        selected = NULL, options = list(maxOptions = 5000)),

      # Season selector
      selectInput("sea", 
        label = "Choose Season (year ending)",
        choices = c("", 2015:1973),
        selected = NULL), 

      # Decimal interval with step value
      sliderInput("pts", "Points:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),
      # Decimal interval with step value
      sliderInput("x2p", "2P Make:", 
                  min = 0, max = 1, value = 0, step= 0.05),
      # Decimal interval with step value
      sliderInput("x2pa", "2P Attempt:", 
                  min = 0, max = 1, value = 0, step= 0.05),
      # Decimal interval with step value
      sliderInput("x3p", "3P Make:", 
                  min = 0, max = 1, value = 0, step= 0.05),
      # Decimal interval with step value
      sliderInput("x3pa", "3P Attempt:", 
                  min = 0, max = 1, value = 0, step= 0.05),
      # Decimal interval with step value
      sliderInput("x3bias", "% of Points from 3:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),		 
      # Decimal interval with step value
      sliderInput("ft", "Free-Throw Make:", 
                  min = 0, max = 1, value = 0, step= 0.05),
      # Decimal interval with step value
      sliderInput("fta", "Free-Throw Attempt:", 
                  min = 0, max = 1, value = 0, step= 0.05),	
      # Decimal interval with step value
      sliderInput("ftr", "Free-Throw Rate:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),
      # Decimal interval with step value
      sliderInput("x2per", "2P %:", 
                  min = 0, max = 1, value = 0, step= 0.05),
      # Decimal interval with step value
      sliderInput("x3per", "3P %:", 
                  min = 0, max = 1, value = 0, step= 0.05),
      # Decimal interval with step value
      sliderInput("ftper", "Free-Throw %:", 
                  min = 0, max = 1, value = 0, step= 0.05),		 
      # Decimal interval with step value      
      sliderInput("efg", "Effective Field-Goal %:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),
      # Decimal interval with step value      
      sliderInput("ts", "True Shooting %:", 
                  min = 0, max = 1, value = 0, step= 0.05),		     
      # Decimal interval with step value
      sliderInput("ast", "Assists:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),
      # Decimal interval with step value
      sliderInput("tov", "Turnovers:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),
      # Decimal interval with step value
      sliderInput("ato", "Assist-Turnover Ratio:", 
                  min = 0, max = 1, value = 0, step= 0.05),
      # Decimal interval with step value
      sliderInput("drb", "Defensive Rebounds:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),		 
      # Decimal interval with step value
      sliderInput("orb", "Offensive Rebounds:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),
      # Decimal interval with step value
      sliderInput("stl", "Steals:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),
      # Decimal interval with step value
      sliderInput("blk", "Blocks:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),
      # Decimal interval with step value
      sliderInput("pfs", "Personal Fouls:", 
                  min = 0, max = 1, value = 0.1, step= 0.05),
      # Decimal interval with step value
      sliderInput("hgt", "Height:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),
      # Decimal interval with step value
      sliderInput("wgt", "Weight:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),
      # Decimal interval with step value
      sliderInput("mpg", "MPG:", 
                  min = 0, max = 1, value = 1, step= 0.05),		 
      # Decimal interval with step value
      sliderInput("age", "Age:", 
                  min = 0, max = 1, value = 1, step= 0.05)
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      tableOutput("ego"),

      tableOutput("cmpz")
    )
  )
))

