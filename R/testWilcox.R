#' @keywords internal 
#' @export
testWilcox<-function(data,
                     test_column, 
                     cols = NULL, 
                     comparisons = NULL, 
                     alt = "two.sided", 
                     exact = FALSE,
                     paired = FALSE,
                     id_col = NULL,
                     show_test_details = TRUE){
  
  require(tidyverse)
  
  # checks + setup
  if (is.grouped_df(data)) {
    warning("`data` has been ungrouped\n")
    data <- ungroup(data)
  }
  
  if (!(test_column %in% names(data))) {
    stop(sprintf("test_column `%s` not found in data.", test_column))
  }
  
  if (is.null(cols)) {
    cols <- data |> select(where(is.numeric)) |> names()
  } else{
    if ((!all(cols %in% names(data)))){
      not_found <- cols[!(cols %in% names(data))]
      stop(sprintf("Column(s) %s not found in data.", paste0("`", not_found,"`", collapse = ", ")))
    }
  }
  
  if (is.null(comparisons)) {
    comparisons <- data[[test_column]] |> unique() |> sort() |> combn(2, simplify = TRUE)
  } else {
    comp_groups <- comparisons |> as.vector() |> unique()
    if (!all(comp_groups %in% data[[test_column]])) {
      not_found <- comp_groups[!(comp_groups %in% data[[test_column]])]
      stop(sprintf("Comparison group(s) %s not found in %s column.", paste0("`", not_found,"`", collapse = ", "), test_column))
    }
    
    if (any(comparisons[1,] == comparisons[2,])) {
      bad_comps <- comparisons[ , comparisons[1,] == comparisons[2,]]
      if (is.null(dim(bad_comps))) {
        stop(sprintf("Invalid comparison: %s. Comparisons must be pairs of different groups.", paste0(bad_comps, collapse = "-")))
      } else {
        stop(sprintf("Invalid comparisons: %s. Comparisons must be pairs of different groups.", paste0(bad_comps[1,], "-", bad_comps[2,], collapse = ", ")))
      }
    }
  }
  
  if (paired) {
    if (is.null(id_col)) {
      stop("Must specify `id_col` when `paired` is TRUE.")
    } 
    
    if (!(id_col %in% names(data))) {
      stop(sprintf("id_col `%s` not found in data.", id_col))
    }
    
    # check each id has at most one for each group in comparisons
    comp_groups <- comparisons |> as.vector() |> unique()
    tmp <- data |> 
      select(all_of(c(id_col, test_column))) |>
      filter(.data[[test_column]] %in% comp_groups) |>
      group_by(.data[[id_col]], .data[[test_column]]) |>
      mutate(.tmp = row_number())
    if (max(tmp$.tmp) > 1) {
      # some ids have two of the same test_column groups
      dups <- filter(tmp, .tmp > 1)[[id_col]]
      stop(sprintf("IDs are not unique for pairs. ID(s) %s have duplicates.", paste0("`", dups, "`", collapse = ", ")))
    }
    
    # check that there are pairs for each comparison
    for (i in 1:ncol(comparisons)) {
      g1 <- comparisons[1,i]
      g2 <- comparisons[2,i]
      tmp2 <- tmp |> 
        filter(.data[[test_column]] == g1 | .data[[test_column]] == g2) |>
        group_by(.data[[id_col]]) |>
        summarise(.tmp = n()) 
      if (max(tmp2$.tmp) == 1) {
        # no pairs for this comparison
        stop(sprintf("No pairs for comparison: %s-%s", g1, g2))
      }
    }
  }
  
  # use purrr::quietly to collect warnings (not errors)
  quiet_wilcox.test <- quietly(wilcox.test)

  out_df <- tibble(group1 = comparisons[1,], group2 = comparisons[2,])
  out_df <- out_df |>
    mutate(results = map2(
      group1,
      group2,
      function(g1, g2) {
        curr_df <- filter(data, .data[[test_column]] %in% c(g1, g2))
        if (paired) {
          pair_df <- curr_df |> 
            select(all_of(id_col)) |> 
            group_by(.data[[id_col]]) |>
            summarise(n = n()) |>
            filter(n == 2)
          pair_ids <- pair_df[[id_col]]
          curr_df <- filter(curr_df, .data[[id_col]] %in% pair_ids) 
          curr_df <- arrange(curr_df, .data[[id_col]]) # arrange so pairs are one after the other
        }
        freq_tab <- table(curr_df[[test_column]])
        curr_df |>
          summarise(group1.n = freq_tab[[g1]],
                    group2.n = freq_tab[[g2]],
                    across(all_of(cols), ~ {
                      x <- curr_df[[cur_column()]][curr_df[[test_column]] == g1]
                      y <- curr_df[[cur_column()]][curr_df[[test_column]] == g2]
                      if (isTRUE(paired)) {
                        message("Running in paired mode.....")
                        list(quiet_wilcox.test(x, y, alternative = alt, paired = TRUE, exact = exact))
                      } else {
                        message("Running in unpaired mode....")
                        # x <- curr_df[[cur_column()]][curr_df[[test_column]] == g1]
                        # y <- curr_df[[cur_column()]][curr_df[[test_column]] == g2]
                        list(quiet_wilcox.test(x, y, alternative = alt, paired = FALSE, exact = exact))
                      }
                    })
                    
                    )
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
    hst <- res_df[[cols[[1]]]][[1]] # first htest of first col in cols
    cat(paste0(
      hst$method, "\n",
      "  Formula: ", "col ~ ", test_column, "\n",
      "  Paired: ", paired, "\n",
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
      "\nWarnings:\n  ",
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
