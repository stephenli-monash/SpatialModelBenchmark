#' @keywords internal 
#' NULL 
### Function to extract log2 FC from all comparisons

get_log2fc <- function(data,
                              cols,
                              group_col,
                              comparisons,
                              paired = FALSE) {
  purrr::map_dfr(comparisons, function(comp) {
    g1 <- comp[1]
    g2 <- comp[2]
    
    curr_df <- dplyr::filter(data, !!sym(group_col) %in% c(g1, g2))
    
    # Calculate log2 fold difference for each column in cols
    diffs <- purrr::map_dbl(cols, function(cell_col) {
      g1_mean <- mean(curr_df[[cell_col]][curr_df[[group_col]] == g1], na.rm = TRUE)
      g2_mean <- mean(curr_df[[cell_col]][curr_df[[group_col]] == g2], na.rm = TRUE)
      log2(g1_mean / g2_mean)
    })
    
    tibble::as_tibble_row(diffs, .name_repair = ~cols) |>
      dplyr::mutate(comparison = paste(g1, g2, sep = "_vs_"))
  }) |>
    dplyr::relocate(comparison)
}


