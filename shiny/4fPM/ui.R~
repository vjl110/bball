library(shiny)

hyb <- read.csv("data/HYB.OUT.csv")
bpm <- read.csv("data/BPM.OUT.csv")
npi <- read.csv("data/NPI.OUT.csv")

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("NBA Four Factor Ratings"),

	  p("Follow me on Twitter for updates", 
    	      a("@VJL_bball", href = "https://twitter.com/VJL_bball")),
	  br(),  
 	  br(),   

    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4, 
          selectInput("dataset", 
                      "Dataset:", 
                      c("HYBRID", "BPM", "NPI"))
      ),
      column(4, 
          selectInput("odnet", 
                      "O/D/NET:", 
                      c("NET", "O", "D"))
      )
    ),
    fluidRow(	    
      column(4, 
    	sliderInput("range", "Seasons:",
                min = 2001, max = 2015, value = c(2001,2015), format="####")
      ),
      column(4, 
    	numericInput("mp", "At least X minutes played:",
                min = 0, max = 4000, value = 500)
      )      
    ),
    # Create a new row for the table.
    fluidRow(
      dataTableOutput(outputId="table")
    )    
  )  
)


