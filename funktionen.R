normalizeX <- function(x){
  a <- (x-minX)/(maxX - minX);
  return(a)
}

normalizeY <- function(x){
  a <- (x-minY)/(maxY - minY);
  return (a)
}

normalizeZ <- function(x){
  a <- (x - minZ)/(maxZ - minZ);
  return (a)
}
  
insertData <- function(){
  anaData <- readline(prompt="Dateinamen eingeben:")
  anaData <- paste(anaData,".csv",sep = "")
  anaData <- read.csv2(anaData, header = T)
  anaData <- data.frame(anaData)
  
  return (anaData)
}


lists_DataFrame <- function(lstX, lstY, lstZ){
  dat <- data.frame(unlist(lstX), unlist(lstY), unlist(lstZ));
  return (dat)
}

normalizeData <- function(setData, columnNames ){
  sdX <- sd(setData$Acceleration.x..m.s.2.)
  sdY <- sd(setData$Acceleration.y..m.s.2.)
  sdZ <- sd(setData$Acceleration.z..m.s.2.)
  
  sdX_normal <- lapply(sdX, normalizeX)
  sdY_normal <- lapply(sdY, normalizeY)
  sdZ_normal <- lapply(sdZ, normalizeZ)
  
  inDataFrame <- lists_DataFrame(sdX_normal,sdY_normal,sdZ_normal)
  
  colnames(inDataFrame) = columnNames
  
  return(inDataFrame)

}
