#MLE Estimator for BTm

library(bbmle)

BT <- function(li, lj) {
  li/(li+lj)
}

BT2 <- function(wij, )
NRL <- read.csv("NRL.csv", row.names = 1)

mle2(BT, start = list(li = 1, lj = 1), data = list(x=NRL))

