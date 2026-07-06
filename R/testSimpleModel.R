#' @keywords internal 
#' @export
testSimpleModel <- function(data, 
                            test_column, 
                            cols = NULL, 
                            comparisons = NULL, 
                            family = "nbinom", 
                            show_test_details = TRUE) {
  
  require(MASS)  
  require(tidyverse)
  
  if (is.grouped_df(data)) {
    warning("`data` has been ungrouped\n")
    data <- ungroup(data)
  }
  
  if (is.null(cols)) {
    cols <- data |> select(where(is.numeric)) |> names()
  } else {
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
  
  if (family != "nbinom" && family != "poisson") {
    stop(sprintf("Family '%s' not included.", family))
  }

  out_df <- tibble(group1 = comparisons[1,], group2 = comparisons[2,])
  if (family == "nbinom") {
    # use purrr::quietly to collect warnings (not errors)
    quiet_glm.nb <- quietly(glm.nb)
    
    out_df <- out_df |>
      mutate(results = map2(
        group1, group2,
        function(g1, g2) {
          curr_df <- filter(data, .data[[test_column]] %in% c(g1, g2))
          freq_tab <- table(curr_df[[test_column]])
          curr_df |>
            summarise(group1.n = freq_tab[[g1]],
                      group2.n = freq_tab[[g2]],
                      across(all_of(cols),
                             ~ list(quiet_glm.nb(
                               as.formula(paste0(cur_column(), " ~ ", test_column)),
                               data = select(curr_df, !all_of(cols) | .x)))))
        })
      ) |>
      unnest(cols = c(results))
  } else if (family == "poisson") {
    # use purrr::quietly to collect warnings (not errors)
    quiet_glm <- quietly(glm)
    
    out_df <- out_df |>
      mutate(results = map2(
        group1, group2,
        function(g1, g2) {
          curr_df <- filter(data, .data[[test_column]] %in% c(g1, g2))
          freq_tab <- table(curr_df[[test_column]])
          curr_df |>
            summarise(group1.n = freq_tab[[g1]],
                      group2.n = freq_tab[[g2]],
                      across(all_of(cols),
                             ~ list(quiet_glm(
                               as.formula(paste0(cur_column(), " ~ ", test_column)),
                               family = poisson(link="log"),
                               data = select(curr_df, !all_of(cols) | .x)))))
        })
      ) |>
      unnest(cols = c(results))
  } else {
    stop(sprintf("Family '%s' not included.", family)) # should technically never reach here
  }
  
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
    mdl <- res_df[[cols[[1]]]][[1]] # first model of first col in cols
    cat(paste0(
      ifelse(family == "nbinom", "glm.nb", "glm"),"\n",
      "  Formula: ", "col ~ ", test_column, "\n",
      "  Family: ", mdl$family$family, "\n",
      "  Link: ", mdl$family$link, "\n",
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
      "\nWarnings from ", ifelse(family == "nbinom", "glm.nb", "glm"),":\n  ",
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