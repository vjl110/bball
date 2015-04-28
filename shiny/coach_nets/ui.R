library(shiny)
library(igraph)

net <- read.csv("data/coach_net.csv")


shinyUI(fluidPage(

verticalLayout(		  
  titlePanel("Coach Connections"),
	  p(h5("Overview:"), "This tool is designed to help visualize the connections between past and present NCAA coaches. Use the controls below to generate a coaching network. Arrows point from a given coach to any head coaches who at one time worked as his assistant or played under him. Names in red indicate currently active head coaches (as of 2014)."),
	  p("Data entry is an ongoing process, so please notify me on", 
	    a("Twitter", href = "https://twitter.com/VJL_bball"), 
	       "if you find any errors or omissions that you would like to see corrected"),  
	  p(h5("Controls:"),
	  em("Coach:"), "Select a focal coach either by searching the list or typing in his name.",  
	  em("Generation:"), "Select the degree of connections you want to see using the 'generation' slider. +1 gives you all coaches that the focal coach has as players or assistants, +2 gives the same along with those who were once players or assistants to the coaches in generation 1.  -1 gives you all of the coaches who the focal coach worked under as a player or assistant, -2 gives the coaches those mentors worked under as well.",
	  em("Size:"), "Some networks are unwieldy at a large size, while others are illegible at a smaller size.  Use the 'size' slider to change the size of the network."),
	  br(),  
 	  br(),

    fluidRow(
  	column(3, wellPanel(
		selectizeInput("nam", 
       	 		label = "Coach",
        		choices = c("", paste(unique(as.character(net$ego)))),
        		selected = "", options = list(maxOptions = 5000))
		)),
  	column(3, wellPanel(		      
      		# Decimal interval with step value
      		sliderInput("gen", "Generation:", 
                 	min = -3, max = 3, value = 0, step= 1)
		)),
  	column(3, wellPanel(		      
      		# Decimal interval with step value
      		sliderInput("sze", "Size:", 
                 	min = 400, max = 1500, value = 400, step= 10)
		))	     
	),
  plotOutput("netPlot")
	
     )		  
))



