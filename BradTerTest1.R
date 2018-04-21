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



BradTer <- function(outcome, player1, player2) {
  # outcome <- matrix of win for/against outcomes
  # player 1 and 2 corresponding to the winner/loser of outcome columns
  K <- length(unique(player1))
  K
  ans1 <- c()
  ans2 <- c()
  p = c(1:K)
  for (i in (1:K)) {
    ans1 <-c(ans1, outcome[i,1]*log(p[i]))
    print(ans1)
  }
  for (i in (1:K)) {
    if (outcome[i, 1] < outcome[i, 2]) 
      ans2 <- c(ans2, sum(outcome[i,])*log(2* p[i]))
      print(ans2)
  }
  ans <- ans1 - ans2
  print(ans)
}

data("citations", package = "BradleyTerry2")
citations.sf <- countsToBinomial(citations)
names(citations.sf)[1:2] <- c("journal1", "journal2")
BradTer(cbind(citations.sf$win1,citations.sf$win2), citations.sf$journal1, citations.sf$journal2)

BTm(cbind(win1, win2), journal1, journal2, data = citations.sf)

