library(shiny)
options(xtable.include.rownames=F)


ss <- read.csv("data/simset.nba.csv")

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
       	   need(input$nam != "" & input$sea != "", "")
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
			input$ato*abs(ss$ATO[ss$Name == input$nam & ss$Season == input$sea] - ss$ATO) +			
			input$stl*abs(ss$STL[ss$Name == input$nam & ss$Season == input$sea] - ss$STL) +
			input$blk*abs(ss$BLK[ss$Name == input$nam & ss$Season == input$sea] - ss$BLK) +
			input$pfs*abs(ss$PF[ss$Name == input$nam & ss$Season == input$sea] - ss$PF) +
			input$hgt*abs(ss$Height[ss$Name == input$nam & ss$Season == input$sea] - ss$Height) + 
			input$wgt*abs(ss$Weight[ss$Name == input$nam & ss$Season == input$sea] - ss$Weight) +
			input$mpg*abs(ss$MPG[ss$Name == input$nam & ss$Season == input$sea] - ss$MPG) +			
			input$age*abs(ss$Age[ss$Name == input$nam & ss$Season == input$sea] - ss$Age))/ (input$pts+input$x2p+input$x2pa+input$x3p+input$x3pa+input$x3bias+input$ft+input$fta+input$ftr+input$x2per+input$x3per+input$ftper+input$efg+input$ts+input$drb+input$orb+input$ast+input$tov+input$ato+input$stl+input$blk+input$pfs+input$hgt+input$wgt+input$mpg+input$age)

      ss$val <- ss$cmb[ss$Name == input$nam & ss$Season == input$sea] - ss$cmb

	cmp <- ss[order(ss$diff), ]
      	cmp <- subset(cmp, Name != input$nam | Season == input$sea)[1:11,]
	data.frame("Name" = cmp$Name, "Season" = cmp$Season, "SD" = cmp$diff, "VAL" = cmp$val, "AGE" = cmp$age, "PTS" = cmp$pts, "eFG" = cmp$efg, "3PA" = cmp$x3pa, "FTR" = cmp$ftr, "TRB" = cmp$trb, "AST" = cmp$ast, "TOV" = cmp$tov, "STL" = cmp$stl, "BLK" = cmp$blk, "PF" = cmp$pf, "HGT" = cmp$hgt, "WGT" = cmp$wgt)
  }) 
  
  # Ego table
  output$ego <- renderTable({
    sliderValues()[1,]
  })
  # comparison players
  output$cmpz <- renderTable({
    sliderValues()[2:11,]
  })
})


