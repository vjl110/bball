library(shiny)

ss <- read.csv("data/simset.csv")

# Define server logic for slider examples
shinyServer(function(input, output, session) {

#	yrs <- as.numeric(ss$Season[ss$Name == input$nam])

  observe({
	# this will change the value
	updateSelectInput(session, "sea", choices = ss$Season[ss$Name == input$nam])
	})	
  
  # Reactive expression to compose a data frame containing all of
  # the values
  sliderValues <- reactive({
      validate(
       	   need(input$nam != "" & input$sea != "", "Please select a player")
      )	   
    
    # Compose data frame
      ss$diff <- (input$pts*abs(ss$PTS[ss$Name == input$nam & ss$Season == input$sea] - ss$PTS) + 
			input$x2p*abs(ss$X2P[ss$Name == input$nam & ss$Season == input$sea] - ss$X2P) +
			input$x2pa*abs(ss$X2PA[ss$Name == input$nam & ss$Season == input$sea] - ss$X2PA) +
			input$x3p*abs(ss$X3P[ss$Name == input$nam & ss$Season == input$sea] - ss$X3P) +
			input$x3pa*abs(ss$X3PA[ss$Name == input$nam & ss$Season == input$sea] - ss$X3PA) +
			input$x3bias*abs(ss$X3bias[ss$Name == input$nam & ss$Season == input$sea] - ss$X3bias) +			
			input$ft*abs(ss$FT[ss$Name == input$nam & ss$Season == input$sea] - ss$FT) +
			input$fta*abs(ss$FTA[ss$Name == input$nam & ss$Season == input$sea] - ss$FTA) +
			input$ftr*abs(ss$FTR[ss$Name == input$nam & ss$Season == input$sea] - ss$FTR) +			
			input$x2per*abs(ss$X2per[ss$Name == input$nam & ss$Season == input$sea] - ss$X2per) +
			input$x3per*abs(ss$X3per[ss$Name == input$nam & ss$Season == input$sea] - ss$X3per) +
			input$ftper*abs(ss$FTper[ss$Name == input$nam & ss$Season == input$sea] - ss$FTper) +			
			input$efg*abs(ss$EFG[ss$Name == input$nam & ss$Season == input$sea] - ss$EFG) +
			input$ts*abs(ss$TS[ss$Name == input$nam & ss$Season == input$sea] - ss$TS) +
			input$drb*abs(ss$DRB[ss$Name == input$nam & ss$Season == input$sea] - ss$DRB) +			
			input$orb*abs(ss$ORB[ss$Name == input$nam & ss$Season == input$sea] - ss$ORB) +
			input$ast*abs(ss$AST[ss$Name == input$nam & ss$Season == input$sea] - ss$AST) +
			input$tov*abs(ss$TOV[ss$Name == input$nam & ss$Season == input$sea] - ss$TOV) +
			input$stl*abs(ss$STL[ss$Name == input$nam & ss$Season == input$sea] - ss$STL) +
			input$blk*abs(ss$BLK[ss$Name == input$nam & ss$Season == input$sea] - ss$BLK) +
			input$pfs*abs(ss$PF[ss$Name == input$nam & ss$Season == input$sea] - ss$PF) +
			input$hgt*abs(ss$Height[ss$Name == input$nam & ss$Season == input$sea] - ss$Height) + 
			input$wgt*abs(ss$Weight[ss$Name == input$nam & ss$Season == input$sea] - ss$Weight) +
			input$age*abs(ss$Age[ss$Name == input$nam & ss$Season == input$sea] - ss$Age))/ (input$pts+input$x2p+input$x2pa+input$x3p+input$x3pa+input$x3bias+input$ft+input$fta+input$ftr+input$x2per+input$x3per+input$ftper+input$efg+input$ts+input$drb+input$orb+input$ast+input$tov+input$stl+input$blk+input$pfs+input$hgt+input$wgt+input$age)

      ss$val <- ss$EWP[ss$Name == input$nam & ss$Season == input$sea] - ss$EWP
     			 
	cmp <- ss[order(ss$diff), ]
	cmp <- subset(cmp, Name != input$nam & Season != input$sea)	
	data.frame("Name" = cmp$Name[1:50], "Season" = cmp$Season[1:50], "Team" = cmp$Tm[1:50], "SD" = cmp$diff[1:50], "VAL" = cmp$val[1:50])
  }) 
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
})


