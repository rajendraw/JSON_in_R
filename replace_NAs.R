library("rjson")

#  Steps :
#1. From the original Df subset the Data Frame that has only the columns "Rooms" and the list of 
#   wifi names specified.
#2. Subset the Df to have only one room. i.e. we are running loop on number of rooms
#3. In this subsetted Df , subset agian per wifi name, i.e. at this stage we have a df that has
#   only one room and one wifi.
#4. Compute the mean of the wifi per that room and replace NAs as asked in the question.
#5. Repeat this for all the other wifis and do a "cbind". At this point, we have a Df for which
#   analysis on one room is completed.
#6. Repeat the process for each room and do a "rbind"

replace_NAs <- function( csvFile = "indoor-location-test.csv", 
                         wifi=c("b0.e7.54.64.62.f9","f8.d1.11.5a.a0.54")) {
  
  origDf <- read.csv(csvFile)
  rooms = as.vector(unique(origDf["Room"]))
  
  x = c("Room", wifi)
  origDf <- subset(origDf, select = x)
  naReplacedDf <- NULL
  
  for (j in c(1:nrow(rooms))) {
    
    df <- subset(origDf, origDf$Room == rooms[j,])
    finalDf <- df["Room"]
    
    for (i in c(1:length(wifi))) {
      
      x2 <- c( wifi[i] )
      
      tempDf <- df[,x2]
      colMean = mean(tempDf, na.rm = T)
      
      if (colMean > -90) {
        tempDf[is.na(tempDf)] <- colMean
      } else {
        tempDf[is.na(tempDf)] <- -100
      }
      
      finalDf <- cbind(finalDf,tempDf)
      tempDf <- NULL
    }
    
    naReplacedDf <- rbind(naReplacedDf,finalDf)
    finalDf <- NULL
  }
  
  colnames(naReplacedDf) <- c("Room", wifi)
  return (naReplacedDf)
  
}

  
  