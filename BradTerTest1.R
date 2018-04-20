# Bradley Terry Implementation with Additional Flexibility

# Rowan Elson-Green - University of Queensland School of Mathematics and Physics

BradTer <- function(outcome = 1, player1, player2, family = "binomial") {
  call <- match.call()
  
  if (is.character(family))
    family <- get(family, mode = "function", envir = parent.frame())
  if (if.function(family))
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("`family' not recognized")
  }
  if (family$family != "binomial")
    stop("`family' must be binomial")
  if(!family$link %in% c("logit", "probit", "cauchit"))
    stop("link for binomial family must be one of \"logit\", \"probit\", or \"cauchit\"")
  fcall <- as.list(match.call(expand.dots = FALSE))
  setup <- match(c("outcome", "player1", "player2"), names(fcall), 0L)
  
  mf <- data.frame(X = setup$player1)
  if (!is.null(setup$X)) { 
    mf$X <- setup$X
    formula <- Y ~ X - 1
  }
  else formula <- Y ~ 0
  mf$Y <- setupY
  
  dotArgs <- fcall$"..."
  if (is.null(setup$random)) {
    method <
    
  }
  }