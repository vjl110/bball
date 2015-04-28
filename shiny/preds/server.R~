library(shiny)

drft <- read.csv("data/OUTPUT.DRAFT.csv")
full <- read.csv("data/OUTPUT.FULL.csv")
	ncaa <- subset(full, League == "NCAA")
	euro <- subset(full, League != "NCAA")

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table <- renderDataTable({
    if(input$dataset == "Draft"){	
    	data <- subset(drft, MP >= input$mp & Season >= as.numeric(input$range[1]) & Season <= as.numeric(input$range[2])) 
    }else if(input$dataset == "NCAA"){
    	data <- subset(ncaa, MP >= input$mp & Season >= as.numeric(input$range[1]) & Season <= as.numeric(input$range[2]))
    }else{
    	data <- subset(euro, MP >= input$mp & Season >= as.numeric(input$range[1]) & Season <= as.numeric(input$range[2]))
    }	
    data
  }
  , options = list(bAutoWidth = FALSE))
  
})



