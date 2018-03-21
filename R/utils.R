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

#' Apply function over selected columns
#' 
#' Select columns using `dplyr`'s `select` function, 
#' and then apply a function to each row. The function
#' default to `sum`. 
#' @param selection Use a `select` helper, e.g. `starts_with()` to selection variables.
#' @param fun A function to apply to each row of selected columns.
#' @author Mark Andrews (mark.andrews@@ntu.ac.uk)
#' @export aggregate
#' @examples
#' set.seed(42)
#' N <- 10
#' Df <- tibble(p_1 = rnorm(N),
#'              p_2 = rnorm(N),
#'              q_1 = rnorm(N),
#'              q_2 = rnorm(N))
#' Df
#' 
#' # mean over p_1 and p_2
#' aggregate(Df, starts_with('p'), mean)
#' 
#' # max over p_1 and q_1
#' aggregate(Df, ends_with('1'), max)
#' 
#' # sum over p_1, p_2, q_1, q_2
#' aggregate(Df, everything())
aggregate <- function(Df, selection, fun=sum){
   select(Df, selection) %>% apply(1, fun)
}


