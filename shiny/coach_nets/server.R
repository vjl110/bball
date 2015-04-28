library(shiny)
library(igraph)

net <- read.csv("data/coach_net.csv")
crnt <- subset(net, crnt == 1)

shinyServer(function(input, output) {

  myheight <-function(){
    input$sze
  }	    
   
  output$netPlot <- renderPlot({
      validate(
       	   need(input$nam != "", "Please select a coach")
      )
      validate(
       	   need(input$gen != 0, "Please move the generation slider")
      )	  
      net <- subset(net, !is.na(par))
      # descendants
      pls1 <- subset(net, par == input$nam) 	
	p1 <- unique(c(as.character(pls1$ego), as.character(pls1$par)))
      pls2 <- subset(net, par %in% p1)
      	p2 <- unique(c(as.character(pls2$ego), as.character(pls2$par)))
      pls3 <- subset(net, par %in% p2)
      #	ancestors
      mns1 <- subset(net, ego == input$nam) 	
	m1 <- unique(c(as.character(mns1$ego), as.character(mns1$par)))
      mns2 <- subset(net, ego %in% m1)
      	m2 <- unique(c(as.character(mns2$ego), as.character(mns2$par)))
      mns3 <- subset(net, ego %in% m2)
	if(input$gen == 1){
		set <- pls1
	}else if(input$gen == 2){
		set <- pls2
	}else if(input$gen == 3){
		set <- pls3		
	}else if(input$gen == -1){
		set <- mns1
	}else if(input$gen == -2){
		set <- mns2
	}else if(input$gen == -3){
		set <- mns3		
	}else{
		deg <- NA
	}
	deg <- graph.data.frame(set, directed=T)

      validate(
       	   need(nrow(set) != 0, "Nothing found")
      )	  
      

    V(deg)$label.color <- ifelse(V(deg)$name %in% crnt[,2], "red", "black")
    plot.igraph(deg, vertex.label=V(deg)$name, layout=layout.graphopt, vertex.size= 0.1, edge.color="grey", edge.width=E(deg)$weight, edge.arrow.size=0.5, edge.curved=FALSE)	    

  }, height = myheight)

})


    


