# Predictor set from BradleyTerry2
# Allows user to find win probabilities for a contest 

library("BradleyTerry2")

BTmpredict <- function(model1, player1, player2) {
  # For the test model (NRL 2017 dataset) player names have a .. in front
  # e.g for player1 to be Broncos, must be ..BRO
  mod <- model1$coefficients
  logodds <- c()
  players <- (1:length(model1$coefficients))
  for (i in players) {
    logodds <- c(logodds, mod[i])
  }
  ans <- logodds[player1]/(logodds[player1]+logodds[player2])
  names(ans) = c()
  cat(player1, "Has a", ans, "chance of beating", player2)
}