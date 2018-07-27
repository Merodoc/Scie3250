#Tests to run for BT datasets
library("BradleyTerry2")
source("BradTerTest2.R")

#NRL <- read.csv("NRL.csv", row.names=1)
#NRL2 <- as.data.frame(table(NRL))
#L <- array(1, 16)
#names(L) <- unique(NRL[,1])
# Run our Version
#X <- RBT(NRL2, L, 0.001)
# Run BradleyTerry2
#NRLModel <- BTm(results, Winner, Loser, data = NRL, refcat="NEW")
#Z <- BTabilities(NRLModel)
# Error
#sort(Z[,1])-sort(X)

data(citations)
citations.sf <- countsToBinomial(citations)
names(citations.sf)[1:2] <- c("journal1", "journal2")
citeModel <- BTm(cbind(win1, win2), journal1, journal2, data = citations.sf, refcat = "Comm Statist")
Y <- BTabilities(citeModel)
counts <- as.data.frame(table(citations.sf))
citations.m <- matrix(c("Biometrika", "Comm Statist", 730, "Biometrika", "JASA", 498, "Biometrika", "JRSS-B", 221, "Comm Statist", "JASA", 68, "Comm Statist", "JRSS-B", 17, "Comm Statist", "Biometrika", 33, "JASA", "JRSS-B", 142, "JASA", "Biometrika", 320, "JASA", "Comm Statist", 813, "JRSS-B", "Biometrika", 284, "JRSS-B", "Comm Statist", 276, "JRSS-B", "JASA", 325), ncol = 3, byrow = TRUE)
players <- length(unique(counts$journal1))
colnames(citations.m) <- c("Winner", "Loser", "Freq")
L <- array(1, players)
names(L) <- unique(counts$journal1)
citations.m <- as.data.frame(citations.m)
citations.m$Freq <- as.numeric(levels(citations.m$Freq)[citations.m$Freq])
H <- RBT(citations.m, L, 0.0001)

#sort(Y[,1]) - sort(H)