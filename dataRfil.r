setwd("C:/Users/Jennifer/Desktop/DVK/HT-16/Exjobb/Filer f√∂r research")

orig_data <- read.table("data.txt", sep = " " , header = F ,
                     na.strings ="", stringsAsFactors= F)
					 
library(plyr)
renamed_data <- rename(orig_data, replace = c("V1"="seqID", "V2"="eventID", "V3"="item"))

aggData <- aggregate(item ~ seqID:eventID, data = renamed_data, toString) #aggregera till String

aggData <- aggregate(item ~ seqID:eventID, data = renamed_data, c) #aggregera till lista
aggData2 <- mutate(aggData, size = base::lengths(item)) #skapa kolumn size med l√§ngd p√• varje lista

colnames(aggData2) <- c("V1", "V2", "V3", "size")
mergedData <- cbind( aggDataString, aggData2 )
mergedData$V1 <- NULL
mergedData$V2 <- NULL
mergedData$V3 <- NULL

mergedData <- mergedData[,c(1,2,4,3)]

max <- apply(mergedData, 2, max) #max v‰rde fˆr varje kolumn,tredje ‰r max fˆr size
mergedDataSep <- separate(mergedData, col = "item", into = paste("V", 1:99, sep = ",")) #99 var max. separera items till 1:max kolumner
sortData<- mergedDataSep[with(mergedDataSep, order(seqID, eventID)), ] #sorta pÂ seqID sen eventID

write.table(mergedDataSep, "data2.txt", na="", row.names=FALSE, col.names=FALSE) #skriv till fil

x <- read_baskets(con = "data2.txt", info = c("sequenceID","eventID","SIZE")) #l‰s in filen som transaction
