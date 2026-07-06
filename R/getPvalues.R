#' @keywords internal 
#' NULL 
getPvalues <- function(results_df, 
                       cols, 
                       adjust = FALSE,
                       adj_mode = "append",
                       adj_method = "bonferroni",
                       adj_grouping = NULL) {
  
  # Note: may be a better way to deal with unknown test/model classes.
  #   - purr::safely?
  #   - add argument for function that extracts p-vaues
  
  require(tidyverse)
  print( results_df)
  pvalue_df <- results_df |> 
    mutate(across(all_of(cols), ~ map_dbl(.x, function(x) {
      if (is(x, "htest") | is(x, "ctest")) {
        x$p.value
      } else if (is(x, "glm")) {
        coeffs <- summary(x)$coefficients
        coeffs[rownames(coeffs) != "(Intercept)", "Pr(>|z|)"]
      } else if (is(x, "glmmTMB")) {
        coeffs <- summary(x)$coefficients$cond
        coeffs[rownames(coeffs) != "(Intercept)", "Pr(>|z|)"]
      } else {
        stop(sprintf("Unrecognised class: %s", class(x)))
      }
    })))
  print( pvalue_df)
  if (adjust) {
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
  }
    
  return(pvalue_df)
}