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

#' Combine csv files
#' 
#' This function stacks csv files. 
#' The log odds of a probability p is log_odds = log(p/(1-p)), so
#' ilogit(log_odds) = p. It is already built in to R with the `plogis`
#' function, but I prefer to code it explicitly and call it `ilogit`.
#' @param data_directory Where all the csv files live
#' @param save_data_frame Write the resulting data frame to csv
#' @author Jens Roeser (jens.roeser@@ntu.ac.uk)
#' @export combine_data
#' @return A data frame that is a row_bind of all csv files in the data directory
#' @examples 
#' d <- combine_data(data_directory = "raw_data")
combine_data <- function(data_directory = "", save_data_frame = TRUE){

  suppressPackageStartupMessages(library(tidyverse, warn.conflicts = FALSE)) # Load useful functions

  options(readr.num_columns = 0)  

  dd <- data_directory
  
  # find all csv files in raw_data and save them in "ids"
  ids <- dir(dd, pattern = ".csv")
  
  if(length(ids)==0){
    print("No CSV data frames in current directoy. Sorry ;)")
  }
  
  # Iterate over each data frame and save them in d
  d <- tibble()
  for(id in ids){
    
    tmp <- paste(dd, id, sep = "/")
    read_csv(tmp) %>%
      mutate(subj = id) %>%
      bind_rows(d) -> d
  }
  
  # Save the collapsed data
  if(save_data_frame){
    write_excel_csv(d, "data.csv")
  }
  return(d)  
}
 
