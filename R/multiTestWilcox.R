#' @keywords internal 
#' @export
multiTestWilcox <- function(data,
                            test_columns, 
                            cols = NULL, 
                            comparisons = NULL, 
                            alt = "two.sided", 
                            exact = FALSE,
                            paired = FALSE,
                            id_col = NULL,
                            show_test_details = TRUE,
                            names_to = "shuffleID") {
  
  # wrapper for running test on multiple test_cols (generally shuffled data)
  # could look consider adding a way to collate the messages from testWilcox()
  # I think it is a good idea to call testWilcox() in this function rather than 
  # re-writing the code here because any changes made to test wilcox will be more
  # will flow into this function too.
  
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
  
  if (paired & is.null(id_col)) {
    stop("Must specify `id_col` when `paired` is TRUE.")
  }
  
  res_list <- list()
  for (test_col in test_columns) {
    res_list[[test_col]] <- testWilcox(data=data,
                                       test_column=test_col, 
                                       cols = cols, 
                                       comparisons = comparisons, 
                                       alt = alt, 
                                       exact = exact,
                                       paired = paired,
                                       id_col = id_col,
                                       show_test_details = show_test_details)
  }
  
  res <- res_list |> enframe(name = names_to, value = ".value") |> unnest(cols = c(.value))
  return(res)
}

  
