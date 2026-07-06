#' @keywords internal 
#' NULL 
testClusWilcox<-function(data,
                         test_column, 
                         clus_id_col,
                         cols = NULL, 
                         comparisons = NULL, 
                         alt = "two.sided", 
                         exact = FALSE,
                         method = "rgl",
                         show_test_details = TRUE){
      
  # NOTES:
  
  require(clusrank)
  require(tidyverse)
  
  if (is.grouped_df(data)) {
    warning("`data` has been ungrouped\n")
    data <- ungroup(data)
  }
  
  if (is.null(cols)) {
    cols <- data |> select(where(is.numeric)) |> names()
  }
  
  if (is.null(comparisons)) {
    comparisons <- data[[test_column]] |> unique() |> sort() |> combn(2, simplify = TRUE)
  }
  
  # use purrr::quietly to collect warnings (not errors)
  quiet_clusWilcox.test <- quietly(clusWilcox.test)
  
  out_df <- tibble(group1 = comparisons[1,], group2 = comparisons[2,])
  out_df <- out_df |>
    mutate(results = map2(
      group1,
      group2,
      function(g1, g2) {
        curr_df <- filter(data, .data[[test_column]] %in% c(g1, g2))
        freq_tab <- table(curr_df[[test_column]])
        curr_df |>
          summarise(group1.n = freq_tab[[g1]],
                    group2.n = freq_tab[[g2]],
                    across(all_of(cols),
                           ~ list(quiet_clusWilcox.test(
                             as.formula(paste0(cur_column(), " ~ ", test_column, " + cluster(",id_col,")")),
                             data = select(curr_df, !all_of(cols) | .x),
                             alternative = alt,
                             exact = exact,
                             method = method))))
      })) |>
    unnest(cols = c(results))
  
  # extract result
  res_df <- out_df |> mutate(across(all_of(cols), ~ map(.x, \(x) x[["result"]])))
  
  # extract warnings
  warn_df <- out_df |> 
    mutate(across(all_of(cols), ~ map(.x, \(x) x[["warnings"]]))) |>
    pivot_longer(cols = all_of(cols), names_to = "cell_type", values_to = "warning") |>
    mutate(empty = map_lgl(warning, is_empty)) |> 
    filter(!empty) |> 
    unnest(cols = c(warning)) |>
    select(!(empty | group1.n | group2.n))
  
  # display test details
  if (show_test_details) {
    cst <- res_df[[cols[[1]]]][[1]] # first ctest of first col in cols
    cat(paste0(
      cst$method, "\n",
      "  Formula: ", "col ~ ", test_column, " + cluster(",id_col,")", "\n",
      "  Method: ", method, "\n",
      "  Alternative: ", alt, "\n",
      "  Exact: ", exact, "\n",
      "  Comparisons:\n    ",
      paste0(1:ncol(comparisons), ": ", comparisons[1, ], "-", comparisons[2, ], collapse = "\n    "),
      "\n"
    ))
  }
  
  if (nrow(warn_df) > 0) {    
    # format warning output
    # add header row
    names_row <- names(warn_df)
    names(names_row) <- names_row
    warn_df <- add_row(warn_df, as_tibble_row(names_row), .before = 1)
    
    width_col1 <- max(map_int(warn_df$cell_type, nchar))
    width_col2 <- max(map_int(warn_df$group1, nchar))
    width_col3 <- max(map_int(warn_df$group2, nchar))
    
    message(paste0(
      "\nWarnings from wilcox.test:\n  ",
      paste0(str_pad(warn_df$cell_type, width_col1, side = "right"),
             "  ",
             str_pad(warn_df$group1, width_col2, side = "right"),
             "  ",
             str_pad(warn_df$group2, width_col3, side = "right"),
             "  ",
             warn_df$warning,
             collapse = "\n  ")
    ))
  }
  
  return(res_df)
} 