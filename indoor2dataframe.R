
library("rjson")
source("get_wifi.R")

process_data <- function(x, wifi=w) {
  # make a small data frame
  room <- x$Properties[[1]][[1]]
  d <- data.frame("Source"="wifi", "Room"=room, "Time"=x$MTime)
  d[wifi] <- rep(NA, length(wifi))
  n <- sapply(x$Quantities, function(x) x$Name)
  v <- sapply(x$Quantities, function(x) x$Number)
  d[n] <- v
  return (d)
}

# Steps:
# 1. Read the json File, It comes a huge list.
# 2. The data we need is inside $Data under the source $Source = wifi.
# 3. Traverse to the location $Source = wifi, get $Data and call "process_data" which accepts $Data
# 4. Do a rbind on the output of process_data()
# 5. Repeat the process on a loop of 'fileLength' and 'numData in each file'

indoor2dataframe <- function( jsonFile = "test.json") {
  
  jsonData <- fromJSON(file=jsonFile, method='C')
  fileLength <- length(jsonData)
  
  w <- get_wifi(jsonFile)
  df <- NULL
  
  for (i in c(1:fileLength)) {
    numData <- length(jsonData[[i]]$Data)
    
    for (j in c(1:numData)) {
      
      if (jsonData[[i]]$Data[[j]]$Source[[1]] == "wifi") {
        
        w1 <- jsonData[[i]]$Data[[j]]
        d <- process_data(w1,w)
        df <- rbind(df,d)
      }
    }
  }
  
  return (df)
}

