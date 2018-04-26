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
  input <- results <- rep(1, nrow(dataset))
  parvec <- c(1:16)
  for (i in 1:K) {
      team1 <- levels(wintable[,1])[i]
      for (j in 1:K) {
        y <- 0
        team2 <- levels(wintable[,2])[j]
        count <- 0
        wij <- 0
  ## algorithm for single comparison
  for (winner in wintable[,1]) {
    count <- count + 1
    x <- count
    if(winner == team1) {
      if(wintable[,2][count] == team2) {
        wij <- wij + wintable[x,3]
        }
      
    }
        y = y + wij*log(input[i]) - wij*log(input[i] + input[j])
        print(y)
  }
      parvec[i] <- y
      print(parvec)
      }
  }
  }

NRL <- read.csv(file = "NRL.csv", row.names = 1)
BradTer(NRL)

