library(shiny)

drft <- read.csv("data/OUTPUT.DRAFT.csv")
	drft$Season <- as.integer(drft$Season)
full <- read.csv("data/OUTPUT.FULL.csv")
	full$Season <- as.integer(full$Season)
		ncaa <- subset(full, League == "NCAA")
		euro <- subset(full, League != "NCAA")

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("Projection Model Outputs"),

	  p("Follow me on Twitter for updates", 
    	      a("@VJL_bball", href = "https://twitter.com/VJL_bball")),	    
	  a("Read the FAQ for a brief description of my metrics and how best to use this tool", 
          href = "http://laynevashro.com/basketball/predsFAQ.html"),
	  br(),  
 	  br(),   

    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4, 
          selectInput("dataset", 
                      "Dataset:", 
                      c("Draft", "Euro", "NCAA"))
      ),	     
      column(4, 
    	sliderInput("range", "Seasons:",
                min = 1990, max = 2015, value = c(1990,2015), sep="")
      ),
      column(4, 
    	numericInput("mp", "At least X minutes played:",
                min = 0, max = 10000, value = 0)
      )      
    ),
    # Create a new row for the table.
    fluidRow(
      dataTableOutput(outputId="table")
    )    
  )  
)


