#' @keywords internal 
#' NULL 
countSignificant <- function(pvalue_df, 
                             grouping = NULL, 
                             cols = NULL,
                             id_cols = NULL,
                             sig_level = 0.05,
                             sig_id_format = "vector",
                             print_sig_ids = FALSE) {
  
  
  # NOTES:
  #   - grouping:
  #       the columns to group the df by before summarising 
  #       i.e. the columns that will be in the final dataframe
  #       if NULL then use existing grouping of df
  #   - cols:
  #       the columns to count the significant values for
  #   - id_cols:
  #       the columns to record in sig_ids
  #       Must not have any overlap with grouping
  #       If NULL then all non-grouping columns are used
  #   - sig_id_format: 
  #       one of c("vector", "string")
  #         "vector": elements of .sig columns are given as a character vectors
  #         "string": elements of .sig columns are pasted together into a string
  #
  
  require(tidyverse)
    
  if (!is.null(grouping)) {
    pvalue_df <- group_by(pvalue_df, across(all_of(grouping)))
  } else {
    grouping <- groups(pvalue_df) |> as.character()
  }
  
  if (is.null(cols)) {
    cols <- pvalue_df |> ungroup() |> select(where(is.numeric)) |> names()
  }
  
  if(!is.null(id_cols)) {
    if(!is_empty(intersect(grouping, id_cols))) {
      # grouping and id_cols must be mutually exclusive
      stop("The column(s): ", paste0(intersect(grouping, id_cols), collapse = ", "), " are present in both grouping of pvalue_df and id_cols\n", "  grouping and id_cols must be mutually exclusive")
    }
  } else {
    # use all non-grouping, non-cols columns as id_cols
    id_cols <- names(pvalue_df)[!names(pvalue_df) %in% c(grouping, cols)]
  }
  
  # ignore any cols that aren't in `grouping`, `cols` or `id_cols`
  pvalue_df <- select(pvalue_df, all_of(c(grouping, cols, id_cols)))
  
  sigcounts <- pvalue_df |>
    mutate(across(all_of(cols),  ~ .x < sig_level)) |> 
    unite(.idx, all_of(id_cols), sep = (", ")) |> 
    summarise(across(all_of(cols), sum, .names = "{.col}.sig_counts"), 
              across(all_of(cols), ~ list(.idx[.x]), .names = "{.col}.sig_ids"),
              .groups = "drop")
  
  if (print_sig_ids) {
    tmp <- sigcounts |> 
      select(!ends_with("sig_counts")) |>
      rename_with(.cols = ends_with("sig_ids"), .fn = \(x) gsub(".sig_ids", "", x)) |> 
      pivot_longer(cols = all_of(cols), names_to = "col_name", values_to = "sig_ids") |>
      mutate(is_sigs = map_lgl(sig_ids, \(x) (length(x) > 0))) |>
      filter(is_sigs) |>
      select(!is_sigs) |>
      unnest(cols = c(sig_ids)) |>
      arrange(across(all_of(c(grouping, "sig_ids"))))
    for (col in cols) {
      cat(col, ":\n  ", sep = "")
      curr_df <- filter(tmp, col_name == col) |>
        select(!col_name) |>
        mutate(across(everything(), ~ str_pad(.x, width = max(nchar(.x))+2, pad = " ", side = "right"))) |>
        unite(lines_to_print, everything(), sep = "")
      cat(paste0(curr_df$lines_to_print, collapse = "\n  "), "\n\n", sep = "")
    }
  }
  
  if (sig_id_format == "string") {
    sigcounts <- sigcounts |> mutate(across(ends_with("sig_ids"), ~ map_chr(.x, \(x) paste0(x, collapse = "; "))))
  }
  
  return(sigcounts)
}


