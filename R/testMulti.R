#' @keywords internal 
#' @export
testMulti <- function(data,
                      test_columns, 
                      test_func,
                      cols = NULL, 
                      comparisons = NULL,
                      names_to = "shuffleID",
                      ...) {
  
  # wrapper for running given test function on multiple test_cols (generally shuffled data)
  
  # Notes:
  #   - could look consider adding a way to collate the messages from the function
  #   - find a way to check if test_func is actually one of the packages test functions?
  
  
  require(tidyverse)
  
  if (is.grouped_df(data)) {
    warning("`data` has been ungrouped\n")
    data <- ungroup(data)
  }
  
  if (is.null(cols)) {
    cols <- data |> select(where(is.numeric)) |> names()
  }
  
  if (is.null(comparisons)) {
    comparisons <- data |> select(all_of(c(test_columns))) |> unlist() |> unique() |> sort() |> combn(2, simplify = TRUE)
  }
  
  res_list <- list()
  for (test_col in test_columns) {
    res_list[[test_col]] <- test_func(data=data,
                                      test_column=test_col, 
                                      cols = cols, 
                                      comparisons = comparisons, 
                                      ...)
  }
  
  res <- res_list |> enframe(name = names_to, value = ".value") |> unnest(cols = c(.value))
  return(res)
}