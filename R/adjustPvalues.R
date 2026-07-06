#' @keywords internal 
#' @export
adjustPvalues <- function(pvalue_df, 
                          cols, 
                          adj_mode = "append",
                          adj_method = "bonferroni",
                          adj_grouping = NULL) {
  
  require(tidyverse)
  

  pvalue_df <- pvalue_df |> nest(df = !all_of(adj_grouping)) # works even if adj_grouping is NULL
  if (adj_mode == "replace") {
    if (length(adj_method) > 1) {
      stop("Only one adjustment method allowed when using 'replace' mode.")
    }
    pvalue_df <- pvalue_df |> 
      mutate(df = map(df, function(x) {
        mutate(x, across(all_of(cols), \(p) p.adjust(p, method = adj_method)))
      })) |>
      unnest(cols = c(df))
    # message("p-values have been adjusted using ", adj_method," method.")
  } else if (adj_mode == "append") {
    # can have multiple methods
    f_list <- map(adj_method, ~ (\(x) p.adjust(x, method = .x)))
    names(f_list) <- adj_method
    pvalue_df <- pvalue_df |> 
      mutate(df = map(df, function(x) {
        mutate(x, across(all_of(cols),
                         f_list,
                         .names = paste0("{.col}.{.fn}")))
      })) |>
      unnest(cols = c(df))
    # message("p-values have been adjusted using ", paste0(adj_method, collapse = ", ")," method(s).")
  } else {
    stop("Adjustment mode `", adj_mode, "` not recognised.\nMust be one of c('replace', 'append').")
  }

  
  return(pvalue_df)
}