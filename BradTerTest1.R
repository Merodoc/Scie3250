# Bradley Terry Implementation with Additional Flexibility

# Rowan Elson-Green - University of Queensland School of Mathematics and Physics

library("BradleyTerry2")

FuncSum1 <- function(f, vector1) {
  ans <- 0
  for (val in vector1) {
    ans = ans + f(val)
  }
  print(ans)
}



BradTer <- function(dataset) {
  # outcome <- arbitrary parameter matrix
  # player 1 and 2 corresponding to the winner/loser of outcome columns
  wintable <- as.data.frame(table(dataset))
  # Converts to table of counts
  K <- length(unique(dataset[,1]))
  testvec <- array(1, 16)/16
  print(testvec)
  parvec <- c(1:16)
  teamnames <- c()
  for (i in 1:K) {
      team1 <- levels(wintable[,1])[i]
      teamnames <- c(teamnames, team1)
      y <- 0
      for (j in 1:K) {
        team2 <- levels(wintable[,2])[j]
        count <- 0

  ## algorithm for single comparison
  for (winner in wintable[,1]) {
    count <- count + 1
    x <- count
    wij <- 0

    if(winner == team1) {
      if(wintable[,2][count] == team2) {
        nij <- wij + wintable[x,3]
      }
        y = y + (nij*log(testvec[i]) - nij*log(testvec[i] + testvec[j]))
        z <- y
    }
    parvec[i] <- z
  }
      }
  }
  names(parvec) <- teamnames
  print(parvec)
}

NRL <- read.csv(file = "NRL.csv", row.names = 1)
BradTer(NRL)

