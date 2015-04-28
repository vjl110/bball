library(shiny)
ss <- read.csv("data/simset.csv")

shinyUI(fluidPage(
  
  #  Application title
  titlePanel("NCAA Player Comparisons"),
  
  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(	 
      # Name selector
      selectizeInput("nam", 
        label = "Choose Player",
        choices = c("", paste(unique(as.character(ss$Name)))),
        selected = "", options = list(maxOptions = 2000)),

      # Season selector
      selectInput("sea", 
        label = "Choose Season",
        choices = c("", 2015:1990),
        selected = ""), 
      # Decimal interval with step value
      sliderInput("ewp", "Value:", 
                  min = 0, max = 1, value = 0, step= 0.05),
      # Decimal interval with step value
      sliderInput("pts", "Points:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),
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
                  min = 0, max = 1, value = 0.5, step= 0.05),
      # Decimal interval with step value
      sliderInput("attRIM", "% of Attempts at Rim:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),
      # Decimal interval with step value
      sliderInput("asd", "% of Makes Assisted:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),		 
      # Decimal interval with step value
      sliderInput("ft", "Free-Throw Make:", 
                  min = 0, max = 1, value = 0, step= 0.05),
      # Decimal interval with step value
      sliderInput("fta", "Free-Throw Attempt:", 
                  min = 0, max = 1, value = 0, step= 0.05),	
      # Decimal interval with step value
      sliderInput("ftr", "Free-Throw Rate:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),
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
                  min = 0, max = 1, value = 0, step= 0.05),
      # Decimal interval with step value      
      sliderInput("ts", "True Shooting %:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),		     
      # Decimal interval with step value
      sliderInput("ast", "Assists:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),
      # Decimal interval with step value
      sliderInput("tov", "Turnovers:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),		      
      # Decimal interval with step value
      sliderInput("trb", "Rebounds:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),
      # Decimal interval with step value
      sliderInput("stl", "Steals:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),
      # Decimal interval with step value
      sliderInput("blk", "Blocks:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),
      # Decimal interval with step value
      sliderInput("pfs", "Personal Fouls:", 
                  min = 0, max = 1, value = 0.2, step= 0.05),
      # Decimal interval with step value
      sliderInput("hgt", "Height:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),
      # Decimal interval with step value
      sliderInput("wgt", "Weight:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),
      # Decimal interval with step value
      sliderInput("reach", "Reach:", 
                  min = 0, max = 1, value = 0, step= 0.05),
      # Decimal interval with step value
      sliderInput("wings", "Wingspan:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),		 
      # Decimal interval with step value
      sliderInput("vert", "Vertical:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),
      # Decimal interval with step value
      sliderInput("sprnt", "Sprint:", 
                  min = 0, max = 1, value = 0.15, step= 0.05),
      # Decimal interval with step value
      sliderInput("aglty", "Agility:", 
                  min = 0, max = 1, value = 0.15, step= 0.05),		 
      # Decimal interval with step value
      sliderInput("age", "Age:", 
                  min = 0, max = 1, value = 1, step= 0.05),
      # Decimal interval with step value
      sliderInput("mov", "MOV:", 
                  min = 0, max = 1, value = 0.25, step= 0.05),		 
      # Decimal interval with step value
      sliderInput("sos", "SOS:", 
                  min = 0, max = 1, value = 0.5, step= 0.05),
      # Decimal interval with step value
      sliderInput("sos", "SOS:", 
                  min = 0, max = 1, value = 0, step= 0.1)
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      tableOutput("values")
    )
  )
))

