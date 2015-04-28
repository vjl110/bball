library(shiny)

hyb <- read.csv("data/HYB.OUT.csv")
bpm <- read.csv("data/BPM.OUT.csv")
npi <- read.csv("data/NPI.OUT.csv")

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table <- renderDataTable({
    if(input$odnet == "O"){
    	colz <- c(1:5,7,9,11,13)
    }else if(input$odnet == "D"){
    	colz <- c(1:4, 6,8,10,12,14)
    }else{
    	colz <- c(1:4, 15:19)
    }    

    if(input$dataset == "HYBRID"){	
    	data <- subset(hyb, MP >= input$mp & Season >= as.numeric(input$range[1]) & Season <= as.numeric(input$range[2]))[colz] 	    
    }else if(input$dataset == "BPM"){	
    	data <- subset(bpm, MP >= input$mp & Season >= as.numeric(input$range[1]) & Season <= as.numeric(input$range[2]))[colz] 
    }else{
    	data <- subset(npi, MP >= input$mp & Season >= as.numeric(input$range[1]) & Season <= as.numeric(input$range[2]))[colz] 
    }

    data <- data[order(data[,ncol(data)], decreasing = T), ] 
    data <- data[order(data$Season, decreasing = T), ] 
    data
	
  	}
  , options = list(bAutoWidth = FALSE))
  
})



