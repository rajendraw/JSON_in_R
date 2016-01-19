
select_wifi_mean <- function( csvFile = "indoor-location-test.csv") {
  
  origDf <- read.csv(csvFile,na.strings = c("NA"))
  wifiMeansDf <- aggregate(origDf, origDf["Room"], mean, na.rm = T)
  
  # Remove columns 1 to 4 as NO analysis has to be done on those
  numericCol <- c(5 : ncol(wifiMeansDf))
  wifiMeansDf <- wifiMeansDf[ ,numericCol]
  
  # Force the output of the sapply to a data frame, 
  d3 <- as.data.frame(sapply( wifiMeansDf, function(x) sum( x > -90, na.rm = T) ))
  colnames(d3) <- 'RoomCount'
  
  d4 <- subset(d3, d3$RoomCount > 7)
  
  return (rownames(d4))
  
}