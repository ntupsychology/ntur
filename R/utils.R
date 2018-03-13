#' The inverse logit function
#' 
#' This function calculates the inverse of the log-odds.
#' The log odds of a probability p is log_odds = log(p/(1-p)), so
#' ilogit(log_odds) = p. It is already built in to R with the `plogis`
#' function, but I prefer to code it explicitly and call it `ilogit`.
#' @param log_odds The logarithm, to base e, of an odds
#' @author Mark Andrews (mark.andrews@@ntu.ac.uk)
#' @export ilogit
#' @return A probability, i.e. the p in log(p/(1-p))
#' @examples 
#' p <- 0.75
#' log_odds <- log(p/(1-p))
#' q <- ilogit(log_odds)
#' p == q # Should be TRUE
ilogit <- function(log_odds){
  1/(1 + exp(-log_odds))
}