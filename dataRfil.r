orig_data <- read.table("data.txt", sep = " " , header = F ,
                     na.strings ="", stringsAsFactors= F)
library(Matrix)
library(arules)
library(arulesSequences)
library(plyr)
renamed_data <- rename(orig_data, replace = c("V1"="seqID", "V2"="eventID", "V3"="item"))

aggDataString <- aggregate(item ~ seqID:eventID, data = renamed_data, toString) #aggregera till strings

aggDataVector <- aggregate(item ~ seqID:eventID, data = renamed_data, c) #aggregera till vector
aggData2 <- mutate(aggDataVector, size = base::lengths(item)) #skapa kolumn size med l�ngd p� varje

colnames(aggData2) <- c("V1", "V2", "V3", "size")
mergedData <- cbind( aggDataString, aggData2 )
mergedData$V1 <- NULL
mergedData$V2 <- NULL
mergedData$V3 <- NULL

mergedData <- mergedData[,c(1,2,4,3)]

#max <- apply(mergedData, 2, max) #max v�rde f�r varje kolumn,tredje �r max f�r size
library(tidyr)
mergedDataSep <- separate(mergedData, col = "item", into = paste("V", 1:99, sep = ",")) #99 var max. separera items till 1:max kolumner
sortData<- mergedDataSep[with(mergedDataSep, order(seqID, eventID)), ] #sorta p� seqID sen eventID

write.table(sortData, "data2.txt", na="", row.names=FALSE, col.names=FALSE) #skriv till fil

x <- read_baskets(con = "data2.txt", info = c("sequenceID","eventID","SIZE")) #l�s in filen som transaction
s2 <- cspade(x, parameter = list(support = 0.01), control = list(verbose = TRUE))

t=s1[is.closed(s2)]

#V�lja en siffra som representerar 1 diagnos. Patienter som diagnostiserats med den skall vara med i testgruppen.
#Totalt 2873 patienter. 25878 sjukhusbes�k.
#Patienter inte diagnostiserats med den skall tillh�ra kontrollgruppen. Siffra: 11.

#DELA UPP I KONTROLLGRUPP OCH TESTGRUPP.
#
sortPatientData <- mergedData[with(mergedData, order(seqID, eventID)), ] #sortera data
matrixData <- as(sortPatientData, "matrix") #g�r om till matrix
patientRows <- aggregate(item ~ seqID, data = matrixData, toString) #ny matrix. en sequence per rad, med alla events i kolumn 2
patientFrameRows <- as(patientRows, "data.frame") #till data frame igen
pattern <- "11|, 11,|, 11"
sum(grepl(pattern,patientFrameRows$item)) #summera antalet rows som inneh�ller 11

#1157 rows inneh�ller 11. N�stan h�lften.
#N�sta steg �r att dela upp raderna. De som inneh�ller 11 �r testgrupp. De som inte inneh�ller 11
# �r kontrollgrupp.
sel <- apply(patientFrameRows[,"item",drop=F],1,function(row) length(grep(pattern,row))>0) #v�lj rows med 11
testData <- patientFrameRows[sel,] #stoppa alla de rows i testData
sel <- apply(patientFrameRows[,"item",drop=F],1,function(row) length(grep(pattern,row))==0) #utan 11
controlData <- patientFrameRows[sel,] #skapa data frame f�r kontrollgrupp

#N�sta steg: Omvandla tillbaka test- och kontrolldata i r�tt format f�r att skriva till fil.
#Testdatan som ska skrivas till fil:
testDataFormatted <- subset(sortPatientData, sortPatientData$seqID %in% as.numeric(testData$seqID))
#Kontrolldatan som ska skrivas till fil:
controlDataFormatted <- subset(sortPatientData, sortPatientData$seqID %in% as.numeric(controlData$seqID))

testDataWrite <- separate(testDataFormatted, col = "item", into = paste("V", 1:99, sep = ",")) 
testDataWrite<- testDataWrite[with(testDataWrite, order(seqID, eventID)), ] #sorta p� seqID sen eventID

controlDataWrite <- separate(controlDataFormatted, col = "item", into = paste("V", 1:99, sep = ",")) 
controlDataWrite<- controlDataWrite[with(controlDataWrite, order(seqID, eventID)), ] #sorta p� seqID sen eventID

#Skriv till fil:
write.table(testDataWrite, "testData.txt", na="", row.names=FALSE, col.names=FALSE) 
write.table(controlDataWrite, "controlData.txt", na="", row.names=FALSE, col.names=FALSE) 