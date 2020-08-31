install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("class")
library(class)
library(plyr)
library(ggplot2)
library(tidyverse)
setwd("C:\\Users\\Theo\\Desktop\\workspace_r\\groundData")


#Erstellung und Einlesen der Trainingsdaten
lstRadwegX_sd <- list()
lstRadwegY_sd <- list()
lstRadwegZ_sd <- list()

lstAsphaltX_sd <- list()
lstAsphaltY_sd <- list()
lstAsphaltZ_sd <- list()

lstKopfsteinX_sd <- list()
lstKopfsteinY_sd <- list()
lstKopfsteinZ_sd <- list()

numoftests <- 10
#Einlesen der einzelnen Testläufe
file_name = "asphalt_"
for (i in 1:numoftests) {
  data_file <- paste(file_name, i, ".csv", sep="")
  rawdata <- read.csv2(data_file, header = T)
  
  #Trennung der drei Achsen in eigene Listen, um sd zu bilden
  xAxis <- rawdata$X..m.s.2.
  yAxis <- rawdata$Y..m.s.2.
  zAxis <- rawdata$Z..m.s.2.
  
  #speicherung der sd jeder Achse in eine Variable
  xAxis_sd = sd(xAxis)
  yAxis_sd = sd(yAxis)
  zAxis_sd = sd(zAxis)
  
  #sd wird in Liste zu zugehörigem Untergrund gespeichert
  lstAsphaltX_sd[i] <- xAxis_sd
  lstAsphaltY_sd[i] <- yAxis_sd
  lstAsphaltZ_sd[i] <- zAxis_sd
}

file_name = "kopfstein_"
for (i in 1:numoftests) {
  data_file <- paste(file_name, i, ".csv", sep="")
  rawdata <- read.csv2(data_file, header = T)
  
  #Trennung der drei Achsen in eigene Listen, um sd zu bilden
  xAxis <- rawdata$Acceleration.x..m.s.2.
  yAxis <- rawdata$Acceleration.y..m.s.2.
  zAxis <- rawdata$Acceleration.z..m.s.2.
  
  #speicherung der sd jeder Achse in eine Variable
  xAxis_sd = sd(xAxis)
  yAxis_sd = sd(yAxis)
  zAxis_sd = sd(zAxis)
  
  #sd wird in Liste zu zugehörigem Untergrund gespeichert
  lstKopfsteinX_sd[i] <- xAxis_sd
  lstKopfsteinY_sd[i] <- yAxis_sd
  lstKopfsteinZ_sd[i] <- zAxis_sd
}


file_name = "radweg_"
for (i in 1:numoftests) {
  data_file <- paste(file_name, i, ".csv", sep="")
  rawdata <- read.csv2(data_file, header = T)
  
  #Trennung der drei Achsen in eigene Listen, um sd zu bilden
  xAxis <- rawdata$Acceleration.x..m.s.2.
  yAxis <- rawdata$Acceleration.y..m.s.2.
  zAxis <- rawdata$Acceleration.z..m.s.2.
  
  #speicherung der sd jeder Achse in eine Variable
  xAxis_sd = sd(xAxis)
  yAxis_sd = sd(yAxis)
  zAxis_sd = sd(zAxis)
  
  #sd wird in Liste zu zugehörigem Untergrund gespeichert
  lstRadwegX_sd[i] <- xAxis_sd
  lstRadwegY_sd[i] <- yAxis_sd
  lstRadwegZ_sd[i] <- zAxis_sd
}
rm(i)

groundTruth <- c("asphalt", "asphalt", "asphalt", "asphalt", "asphalt", "asphalt", "asphalt", "asphalt", "asphalt", "asphalt","kopfstein", "kopfstein", "kopfstein", "kopfstein", "kopfstein", "kopfstein", "kopfstein", "kopfstein", "kopfstein", "kopfstein", "radweg", "radweg", "radweg", "radweg", "radweg", "radweg", "radweg", "radweg", "radweg", "radweg")

#zusammenfassen der Achsen in eine Liste
xSD <- c(lstAsphaltX_sd, lstKopfsteinX_sd, lstRadwegX_sd)
ySD <- c(lstAsphaltY_sd, lstKopfsteinY_sd, lstRadwegY_sd)
zSD <- c(lstAsphaltZ_sd, lstKopfsteinZ_sd, lstRadwegZ_sd)
columns <- c("groundTruth", "x", "y", "z")

#minima und maxima der einzelnen Achsen in Variablen abspeichern
minX = min(unlist(xSD))
maxX = max(unlist(xSD))

minY = min(unlist(ySD))
maxY = max(unlist(ySD))

minZ = min(unlist(zSD))
maxZ = max(unlist(zSD))

#normalisierte Listen in neuer Liste abspeichern
xSD_normal <- lapply(xSD, FUN = function(x) (x-minX)/(maxX - minX))
ySD_normal <- lapply(ySD, FUN = function(x) (x-minY)/(maxY - minY))
zSD_normal <- lapply(zSD, FUN = function(x) (x-minZ)/(maxZ - minZ))

#DataFrame wird mit normalisierten Listen erstellt um in knn-Funktion einzufügen
kData <- data.frame(unlist(groundTruth),unlist(xSD_normal),unlist(ySD_normal),unlist(zSD_normal))
colnames(kData) = columns
columns <- c("x", "y", "z")
kData_trainlabels <- kData[ 1:30 ,1]
kData_train <- kData[1:30,2:4]


#Einlesen der Datei mit den Daten(muss im entsprechendem working directory gespeichert sein)
anaData <- insertData()

#Berechnung wie viele Reihen der Datentabelle sich in dem gewählten zu unterteilenden Zeitraum befinden
sizeOfSet <- as.double(readline(prompt = "Abschnittsgröße in Sekunden eingeben:"))
numLoop <- as.integer(max(anaData$ï..Time..s.)/sizeOfSet)

a <- as.double(0)
b <- sizeOfSet

#Erstellung eines Dataframes in dem die Standardabweichungen de Testdaten gespeichert werden sollen
dataInSD <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(dataInSD) <- columns


for (j in 1:numLoop){
  
  testDataSet <- subset(anaData, anaData$ï..Time..s. < b & anaData$ï..Time..s. >= a )
  testDataSet <- normalizeData(testDataSet, columns)
  dataInSD <- rbind(dataInSD, testDataSet)
  
  a = a + sizeOfSet
  b = b + sizeOfSet
}
testDataSet_labeled <- knn(train = kData_train, test = dataInSD, cl = kData_trainlabels , k=5)
testDataSet_labeled






