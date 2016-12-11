#Fil f�r tester p� testdatan!
setwd("C:/Users/Jennifer/Desktop/DVK/HT-16/Exjobb/Filer f�r research")

library(arules)
library(arulesSequences)

x <- read_baskets(con = "testData.txt", info = c("sequenceID","eventID","SIZE")) #läs in filen som transaction
s1 <- cspade(x, parameter = list(support = 0.01), control = list(verbose = TRUE)) #hitta frekventa