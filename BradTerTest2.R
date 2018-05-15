# Implementation of Hunter 2004 iterative algorithm to maximize lambda

Wi <- function(data) {
  # Create arbitrary intial vector
  K <- length(unique(data[,1]))
  # K is number of elements for comparison (Caron and Doucet)
  lambda1 = array(1, K)
  # labmda = {lambda_i}_(i=1)^K
  teamnames <- unique(data[,1])
  wi <- lambda1
  names(wi) <- teamnames
  for (team in teamnames) {
    wins <- which(data[,1]==team)
    wij <- data[wins,]
    wincount <- data[wins,3]
    wi[team] <- sum(wincount)
    # wi is total wins by i
  }
  return(wi)
}

Wij <- function(i, j, data) {
  #For a given i and j, generates the number of times i has beaten j, data should be provided as a counts table
  iloc <- which(data[,1] == i)
  jloc <- which(data[,2] == j)
  # Generates vectors defining the locations where i is the winner and j is the loser
  ij <- intersect(iloc, jloc)
  # Finds the common location between the two location vectors
  wij <- data[ij,3]
  return(wij)
  # Returns the count at the common location
}

Nij <- function(i, j, data) {
  # For a given i and j, generates the number of times i has faced j
  wij <- Wij(i, j, data)
  wji <- Wij(j, i, data)
  return(wij + wji)
}

Hun1 <- function(i, j, data, L, k=1) {
  nij <- Nij(i, j, data)
  li <- L[i]
  lj <- L[j]
  return(nij/(li^k + lj^k))
  }

liIter <- function(i, data, L, k=1) {
  wi <- Wi(data)
  liVec <- c()
  for (j in names(L)) {
    if (j != i) {
      liVec <- c(liVec, Hun1(i, j, data, L, k))
    }
  }
  liSum <- sum(liVec)
  return(wi[i] * liSum^-1)
}

PlanB <- function(data, L, tol, k=1) {
  ansL <- L
  for (i in names(L)) {
    L[i] <- liIter(i, data, ansL, k)
  }
  L <- L*(1/min(L))
  if (length(which((L - ansL) < tol)) == length(L)) {
    return(L)
  }
  else {
  PlanB(data, L, k+1)
  }
}

Lange2007 <- function(i, data, L, k = 1) {
  testvec <- c()  
  for (j in names(L)) {  
    if (Hun1(i, j, data, L, k) != 0) {
      testvec <- c(testvec, Wi(data)[i]/(Hun1(i, j, data, L, k)))
    }
    }
  L[i] <- sum(testvec)
}

RBT <- function(data, L, tol, k=1){
  ansL <- L
  for (i in names(L)) {
    L[i] <- liIter(i, data, ansL, k)
  }
  L <- L*(1/min(L))
  if (length(which((log(L) - log(ansL)) < tol)) == length(L)) {
    return(log(L))
  }
  else {
    RBT(data, L, tol, k)
  }
}




NRL <- read.csv("NRL.csv", row.names=1)
NRL2 <- as.data.frame(table(NRL))
#Wij("BRO", "CAN", as.data.frame(table(NRL)))
#Nij("BRO", "CAN", as.data.frame(table(NRL)))
L <- array(1, 16)
names(L) <- unique(NRL[,1])
#liIter("MEL", NRL, L)
#RBT("BRO", NRL, L)
#Z <- PlanB(NRL, L, 0.00001)
X <- RBT(NRL2, L, 0.00001)
print(X)
results <- rep(1, nrow(NRL))
NRLModel <- BTm(results, Winner, Loser, data = NRL, refcat="NEW")
Z <- BTabilities(NRLModel)
print(Z)
sort(Z[,1])-sort(X)
