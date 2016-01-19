library("rjson")

get_wifi <- function( jsonFile = "indoor-location-training.json") {
  
  jsonData <- fromJSON(file=jsonFile, method='C')
  fileLength <- length(jsonData)
  wifiList <- NULL
  
  for (ii in c(1:fileLength)) {
    
    numData <- length(jsonData[[ii]]$Data)
    
    for (jj in c(1:numData)) {
      
      if (jsonData[[ii]]$Data[[jj]]$Source[[1]] == "wifi") {
      
        numQuantities = length(jsonData[[ii]]$Data[[jj]]$Quantities)
       
        for (kk in c(1:numQuantities)) {
          index <- length(wifiList) + 1
          wifiList[index] <- jsonData[[ii]]$Data[[jj]]$Quantities[[kk]]$Name[[1]]
        }
        wifiList <- unique(wifiList)
      }
    }
  }

  return (sort(wifiList))
}


