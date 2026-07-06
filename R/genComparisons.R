#' @keywords internal 
#' @export
genComparisons <- function(test_column_vec) {
  # test_column_vec is a character vector containing all possible group options i.e. data$test_column
  
  require(tidyverse)
  
  cmps <- test_column_vec |> unique() |> sort() |> combn(2, simplify = TRUE)
  rownames(cmps) <- c("group1", "group2")
  
  return(cmps)
}