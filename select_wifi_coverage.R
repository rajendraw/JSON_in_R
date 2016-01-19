
FindPercentage <- function(x) {
  len = length(x)
  numNAs = sum(is.na(x))
  
  return (100 * (len - numNAs)/len)
}

select_wifi_coverage <- function( csvFile = "indoor-location-test.csv") {
  
  origDf <- read.csv(csvFile,na.strings = c("NA"))
  wifiPercentages <- aggregate(origDf, origDf["Room"], FindPercentage)
  
  numericCol <- c(5 : ncol(wifiPercentages))
  wifiPercentages <- wifiPercentages[ ,numericCol]
  
  d3 <- as.data.frame(sapply( wifiPercentages, function(x) sum( x > 70, na.rm = T) ))
  colnames(d3) <- 'RoomCount'
  
  d4 <- subset(d3, d3$RoomCount > 7)
  
  return (rownames(d4))
  
}