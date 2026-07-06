#' Build per-facet significance label from pval_df and create violin plot
#' @export

build_labels <- function(pval_df, lfc_df, test_cols, g1, g2) {

  filtered_pval <- pval_df |>
    dplyr::filter(group1 == g1, group2 == g2)

  cat("Rows after pval filter:", nrow(filtered_pval), "\n")

  # --- FIX: Vector-safe routing check using %in% ---
  # If "wilcox_pval_adj" is being requested, prioritize the wilcox LFC rows.
  target_type <- if ("wilcox_pval_adj" %in% test_cols) "wilcox" else "glmm"
  
  filtered_lfc <- lfc_df |>
    dplyr::filter(group1 == g1, group2 == g2, model_type == target_type) |>
    dplyr::select(cell_type, LFC)   # drop group cols to avoid suffix conflicts

  cat("Rows in filtered_lfc:", nrow(filtered_lfc), "\n")

  filtered_pval |>
    dplyr::left_join(filtered_lfc, by = "cell_type") |>
    dplyr::mutate(
      lfc_str = dplyr::case_when(
        is.na(LFC) ~ "LFC=NA",
        LFC > 0    ~ paste0("(+) LFC=", round(LFC, 2)),   
        TRUE       ~ paste0("(-) LFC=", round(LFC, 2))    
      ),

      pval_str = purrr::map_chr(cell_type, function(ct) {
        row <- pval_df |>
          dplyr::filter(cell_type == ct, group1 == g1, group2 == g2)

        if (nrow(row) == 0) return("no data")

        parts <- purrr::map_chr(test_cols, function(col) {
          p <- row[[col]]
          if (length(p) > 0 && !is.na(p)) {
            paste0(gsub("_pval_adj$", "", col), " p=", signif(p, 2))
          } else {
            paste0(gsub("_pval_adj$", "", col), " p=NA")
          }
        })

        paste(parts, collapse = " | \n ")
      }),

      full_label = paste0(lfc_str, "\n", pval_str)
    ) |>
    dplyr::select(cell_type, full_label)
}
# --- Core plotting function ---
render_violin_plot <- function(plot_data, label_df, measure_name, title, color_map = NULL,user_params=NULL) {

  df_plot <- plot_data |>
    dplyr::filter(Measure == measure_name) |>
    dplyr::left_join(label_df, by = "cell_type")

  if (nrow(df_plot) == 0) {
    message("No significant results for measure: ", measure_name)
    return(invisible(NULL))
  }

  all_groups <- unique(df_plot$Group)
  n_groups   <- length(all_groups)

  label_pos <- df_plot |>
    dplyr::group_by(cell_type) |>
    dplyr::summarise(full_label = dplyr::first(full_label), .groups = "drop")

  g <- ggplot(df_plot, aes(x = Group, y = Value, colour = Group)) +
    geom_violin(alpha = 0.6, trim = FALSE, fill = "white") +
    geom_jitter(width = 0.1, alpha = 0.2, size = 0.5) +
    geom_text(
      data        = label_pos,
      aes(label   = full_label),
      x           = (n_groups + 1) / 2,
      y           = Inf,
      vjust       = 1.1,
      size        = 2.3,
      fontface    = "bold",
      inherit.aes = FALSE
    ) +
    facet_wrap(~ cell_type, scales = "free_y", ncol = 4) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.4))) +
    theme_bw() +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 6),
      panel.spacing.y = unit(2.5, "lines")
    ) +
    labs(
      title    = title,
      subtitle = paste("Significant results |", user_params$test_column),
      y        = measure_name,
      fill     = user_params$test_column
    )

  # Apply shared colour scale if provided
  if (!is.null(color_map)) {
    g <- g + scale_colour_manual(values = color_map)
  }

  print(g)
}
