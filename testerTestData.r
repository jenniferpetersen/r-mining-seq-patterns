#Fil för tester på testdatan!
setwd("C:/Users/Jennifer/Desktop/DVK/HT-16/Exjobb/Filer för research")

library(arules)
library(arulesSequences)

x <- read_baskets(con = "testData.txt", info = c("sequenceID","eventID","SIZE")) #lÃ¤s in filen som transaction
s1 <- cspade(x, parameter = list(support = 0.01), control = list(verbose = TRUE)) #hitta frekventa