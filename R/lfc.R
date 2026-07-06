compute_lfc <- function(data, colWilcox, colGLMM, pseudocount = 1e-6) {

  compute_lfc_for_suffix <- function(suffix) {
    data_long <- data |>
      tidyr::pivot_longer(
        cols      = tidyr::ends_with(suffix),
        names_to  = "Cell_Type",
        values_to = "Value"
      ) |>
      dplyr::mutate(
        Cell_Type = gsub(paste0(suffix, "$"), "", Cell_Type),
        Value     = as.numeric(Value)        # <-- force numeric
      )

    means <- data_long |>
      dplyr::group_by(Cell_Type, Group) |>
      dplyr::summarise(mean_val = mean(Value, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(mean_val = as.numeric(mean_val))   # <-- force numeric
  print( means)
    groups <- as.character(unique(data$Group))
    groups <- groups[!is.na(groups) & groups != ""]

    pairs_df           <- as.data.frame(t(combn(sort(groups), 2)))
    colnames(pairs_df) <- c("group1", "group2")
   
    lfc_grid <- tidyr::expand_grid(Cell_Type = unique(means$Cell_Type), pairs_df) |>
      dplyr::left_join(means, by = c("Cell_Type", "group1" = "Group")) |>
      dplyr::rename(mean1 = mean_val) |>
      dplyr::left_join(means, by = c("Cell_Type", "group2" = "Group")) |>
      dplyr::rename(mean2 = mean_val) |>
      dplyr::mutate(
        mean1 = as.numeric(mean1),
        mean2 = as.numeric(mean2),
        LFC   = log2((mean2 + pseudocount) / (mean1 + pseudocount))
      )

    list(lfc_grid = lfc_grid, means = means)
  }

  list(
    glmm   = compute_lfc_for_suffix(colGLMM),
    wilcox = compute_lfc_for_suffix(colWilcox)
  )
}