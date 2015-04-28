# Install these packages if you haven't done so already:
# install.packages("RCurl")
# install.packages("RJSONIO")

# load packages
library(RCurl)
library(RJSONIO)

# Choose where you want to save the files by changing the directory below
DIRECTORY <- "~/nbadata/sportvu/"

urls <- c( "http://stats.nba.com/js/data/sportvu/2014/touchesTeamData.json",
            "http://stats.nba.com/js/data/sportvu/2014/defenseTeamData.json",
            "http://stats.nba.com/js/data/sportvu/2014/drivesTeamData.json",
            "http://stats.nba.com/js/data/sportvu/2014/passingTeamData.json",
            "http://stats.nba.com/js/data/sportvu/2014/pullUpShootTeamData.json",
            "http://stats.nba.com/js/data/sportvu/2014/reboundingTeamData.json",
            "http://stats.nba.com/js/data/sportvu/2014/shootingTeamData.json",
            "http://stats.nba.com/js/data/sportvu/2014/speedTeamData.json",
            "http://stats.nba.com/js/data/sportvu/2014/catchShootTeamData.json" )

filenames <- c( "touchesTeamData.csv",
            "defenseTeamData.csv",
            "drivesTeamData.csv",
            "passingTeamData.csv",
            "pullUpShootTeamData.csv",
            "reboundingTeamData.csv",
            "shootingTeamData.csv",
            "speedTeamData.csv",
            "catchShootTeamData.csv" )

getData <- function( n ){
  url <- urls[n]
  filename <- filenames[n]
  data_json <- getURL( url )
  data_list <- fromJSON( data_json )
  number_of_rows <- length( data_list$resultSets[[1]]$rowSet )
  data_table <- data.frame( matrix( unlist( data_list$resultSets[[1]]$rowSet ), nrow=number_of_rows, byrow=T ) )
  colnames( data_table ) <- data_list$resultSets[[1]]$headers
  write.csv(data_table, paste( DIRECTORY, filename, sep=""), row.names=F)
}

for( n in 1:length( urls ) ){
  getData(n)
}
