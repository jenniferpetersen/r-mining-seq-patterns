library(arules)
library(arulesSequences)
library(permute)
library(tidyr)
library(dplyr)
library(compare)

x <- read_baskets(con = "data2.txt", info = c("sequenceID","eventID","SIZE")) #read file as transaction
#frequentTestData <- cspade(x, parameter = list(support = 0.01), control = list(verbose = TRUE)) #find frequent
#frequentTestData <- as(frequentTestData,"data.frame")
y <- as(x,"data.frame")
y[11862, 3] = 1

#permute the sequences
#run cspade again
#repeat 100 times total

agg <- aggregate(eventID ~ sequenceID, data = y, c) #aggregera eventIDs till en rad per seqID

#randomisera chars i varje str�ng
#?
agg$eventID <- lapply(agg$eventID,sample,replace=F) #verkar funka. randomiserar vector.
agg$eventID <- lapply(agg$eventID,toString) #g�r till str�ngar f�r att kunna anv�nda separate_rows l�ttare

#splitta varje eventID i flera rader.
separated <- separate_rows(agg, eventID, sep = ",") #separera eventID till flera rader. 
#separate_rows ger fler rader �n i y!!! Varf�r???

seperated_count <- aggregate(separated$eventID ~ separated$sequenceID, separated, length)
y_count <- aggregate(y$eventID ~ y$sequenceID, y, length)
which(seperated_count != y_count, arr.ind=TRUE)
# Rad 1326 i y har 1 eventID (som �r 49), men 49 stycken eventID i separated & agg

#N�sta steg: merga med originaldata, ny eventID g�ller, ta bort gamla eventID.
#K�r cspade igen.

#B�rja med att d�pa om gamla eventID s� att den �r l�tt att ta bort
colnames(y) <- c("items", "oldSeqID", "oldEvent", "size")
#Merga de tv�
mergedData <- cbind(y, separated) #funkar ej d� separated av n�n anledning har 48 st fler rader nu.

mergedData$oldSeqID <- NULL
mergedData$oldEvent <- NULL
mergedData <- mergedData[,c(1,3,4,2)]

mergedData$eventID <- as.numeric(mergedData$eventID)
mergedData <- mergedData[order(mergedData$sequenceID, mergedData$eventID) ,]
mergedData <- mergedData[,c(2,3,4,1)]

#Next: ber�kna freq, och randomisera igen. 100 g�nger totalt.

write.table(mergedData, "newRandoms.txt", na="", row.names=FALSE, col.names=FALSE) 

x <- read_baskets(con = "newRandoms.txt", info = c("sequenceID","eventID","SIZE")) #read file as transaction
frequent <- cspade(x, parameter = list(support = 0.01), control = list(verbose = TRUE)) #find frequent
frequent <- as(frequent,"data.frame")






