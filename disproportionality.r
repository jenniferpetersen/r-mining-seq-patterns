#Fil för tester på testdatan!

library(arules)
library(arulesSequences)

x <- read_baskets(con = "testData.txt", info = c("sequenceID","eventID","SIZE")) #lÃ¤s in filen som transaction
frequentTestData <- cspade(x, parameter = list(support = 0.01), control = list(verbose = TRUE)) #hitta frekventa

y <- read_baskets(con = "controlData.txt", info = c("sequenceID","eventID","SIZE")) #lÃ¤s in filen som transaction
frequentControlData <- cspade(y, parameter = list(support = 0.01), control = list(verbose = TRUE)) #hitta frekventa

mergedf <- merge(frequentTestData,frequentControlData,by="sequence")
colnames(mergedf) <- c("sequence","testSupport","controlSupport")

mergedf$disproportionality <- mergedf[["testSupport"]]/mergedf[["controlSupport"]] #räkna ut disproportionalitet.
