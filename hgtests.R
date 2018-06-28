library("BradleyTerry2")

NRL <- read.csv(file = "NRL2018.csv", header = TRUE)

#Basic Model

NRLModel1 <- BTm(cbind(home.wins, away.wins), home.team, away.team, data = NRL, id = "team", refcat = "PAR")
summary(NRLModel1)

NRL$home.team <- data.frame(team = NRL$home.team, at.home = 1)
NRL$away.team <- data.frame(team = NRL$away.team, at.home = 0)

NRLModel2 <- update(NRLModel1, formula = ~team + at.home)
summary(NRLModel2)

BTabilities(NRLModel1)
BTabilities(NRLModel2)