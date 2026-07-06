#' Directional heatmap (stacked by comparison, shared scale)
#' @export
pvalueHeatmap_directional <- function(
  pvalue_df,
  lfc_df   = NULL,   # kept for back-compat: single LFC for all columns
  lfc_list = NULL,   # NEW: named list, e.g. list(wilcox = ..., glmm = ...)
  cols = NULL,
  group_cols = c("group1", "group2"),
  sig_levels = c(0.05, 0.01, 0.001),
  scale_colors = c("blue", "white", "red"),
  na_color = "grey90",
  draw_heatmap = FALSE,
  p_min = 1e-10,
  comparison_colors = NULL
) {

  library(dplyr)
  library(tidyr)
  library(ComplexHeatmap)
  library(circlize)
  library(grid)
  library(viridisLite)

  # ----------------------------
  # 1. Detect model columns
  # ----------------------------
  if (is.null(cols)) {
    cols <- grep("_pval_adj$", colnames(pvalue_df), value = TRUE)
  }
  if (length(cols) == 0) stop("No *_pval_adj columns found")

  model_names <- gsub("_pval_adj", "", cols)

  # ----------------------------
  # 2. Resolve per-column LFC sources
  #    lfc_list names must match model_names (e.g. "wilcox", "glmm")
  #    Fall back to lfc_df for any column not in lfc_list
  # ----------------------------
  if (is.null(lfc_list) && is.null(lfc_df)) {
    stop("Provide either lfc_df (single LFC) or lfc_list (per-model LFC).")
  }

  # Normalise lfc_list entries: rename Cell_Type -> cell_type
  normalise_lfc <- function(df) {
    if ("Cell_Type" %in% colnames(df)) df <- dplyr::rename(df, cell_type = Cell_Type)
    if (!"LFC" %in% colnames(df)) stop("lfc data must contain a column named 'LFC'")
    df
  }

  if (!is.null(lfc_list)) {
    lfc_list <- lapply(lfc_list, normalise_lfc)
  }
  if (!is.null(lfc_df)) {
    lfc_df <- normalise_lfc(lfc_df)
  }

  # Map each model column -> its LFC data frame
  col_lfc_map <- lapply(model_names, function(mn) {
    if (!is.null(lfc_list) && mn %in% names(lfc_list)) {
      lfc_list[[mn]]
    } else if (!is.null(lfc_df)) {
      lfc_df
    } else {
      stop("No LFC source found for model '", mn, "'.")
    }
  })
  names(col_lfc_map) <- model_names

  # ----------------------------
  # 3. Merge pvalue_df with EACH unique LFC source; keep only what's needed
  #    We need one merged df per LFC source for the join, but we build
  #    per-cell signed scores column-by-column inside the loop, so we
  #    just pre-join once per unique lfc source.
  # ----------------------------
  unique_lfc_sources <- unique(col_lfc_map)   # deduplicate by identity

  merge_with_lfc <- function(lfc) {
    merged <- dplyr::left_join(pvalue_df, lfc, by = c("cell_type", "group1", "group2"))
    dup <- merged %>%
      dplyr::group_by(cell_type, group1, group2) %>%
      dplyr::filter(dplyr::n() > 1)
    if (nrow(dup) > 0) {
      warning("Duplicate rows after join — keeping first occurrence.")
      merged <- merged %>%
        dplyr::group_by(cell_type, group1, group2) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
    }
    merged
  }

  # Build a lookup: for each model_name, the fully-merged df
  merged_map <- lapply(col_lfc_map, merge_with_lfc)

  # Use the first merged df to drive comparisons / cell types / group colours
  # (all share the same pvalue_df backbone, so comparisons are identical)
  df_ref <- merged_map[[1]]

  # ----------------------------
  # 4. Build group colour palette
  # ----------------------------
  all_groups <- sort(unique(c(df_ref$group1, df_ref$group2)))

  if (!is.null(comparison_colors)) {
    missing_levels <- setdiff(all_groups, names(comparison_colors))
    if (length(missing_levels) > 0) {
      warning("comparison_colors missing levels: ",
              paste(missing_levels, collapse = ", "), " — filling from viridis.")
      extra <- setNames(viridisLite::viridis(length(missing_levels), option = "D"),
                        missing_levels)
      comparison_colors <- c(comparison_colors, extra)
    }
    group_colors <- comparison_colors[all_groups]
  } else {
    group_colors <- setNames(viridisLite::viridis(length(all_groups), option = "D"),
                             all_groups)
  }

  # ----------------------------
  # 5. Unique comparisons & cell types
  # ----------------------------
  comps          <- df_ref %>% dplyr::distinct(group1, group2)
  all_cell_types <- sort(unique(df_ref$cell_type))

  mat_list    <- vector("list", nrow(comps))
  sig_list    <- vector("list", nrow(comps))
  group1_list <- character(nrow(comps))
  group2_list <- character(nrow(comps))

  # ----------------------------
  # 6. Build matrices per comparison
  # ----------------------------
  for (i in seq_len(nrow(comps))) {

    g1 <- comps$group1[i]
    g2 <- comps$group2[i]

    # Skeleton with all cell types
    skeleton <- data.frame(cell_type = all_cell_types)

    # For each model column, pull the signed -log10(p) using the RIGHT LFC
    mat_cols <- lapply(seq_along(cols), function(j) {
      col <- cols[j]
      mn  <- model_names[j]

      sub_df <- merged_map[[mn]] %>%
        dplyr::filter(group1 == g1, group2 == g2) %>%
        dplyr::arrange(cell_type)

      sub_df <- dplyr::left_join(skeleton, sub_df, by = "cell_type")

      p_vec <- sub_df[[col]]
      p_vec[is.na(p_vec)] <- 1
      p_vec[p_vec < p_min] <- p_min

      sign(sub_df$LFC) * (-log10(p_vec))
    })

    mat <- do.call(cbind, mat_cols)
    rownames(mat) <- all_cell_types
    colnames(mat) <- model_names

    mat_list[[i]]    <- mat
    group1_list[[i]] <- g1
    group2_list[[i]] <- g2

    # Significance stars — p-values are model-specific too
    sig_mat <- matrix("", nrow = length(all_cell_types), ncol = length(cols))
    for (j in seq_along(cols)) {
      col <- cols[j]
      mn  <- model_names[j]

      sub_df <- merged_map[[mn]] %>%
        dplyr::filter(group1 == g1, group2 == g2) %>%
        dplyr::arrange(cell_type)
      sub_df <- dplyr::left_join(skeleton, sub_df, by = "cell_type")

      p_vec <- sub_df[[col]]
      p_vec[is.na(p_vec)] <- 1

      for (k in seq_along(sig_levels)) {
        sig_mat[p_vec <= sig_levels[k], j] <- paste(rep("*", k), collapse = "")
      }
    }

    sig_list[[i]] <- sig_mat
  }

  # ----------------------------
  # 7. Global color scale
  # ----------------------------
  all_vals    <- unlist(mat_list)
  finite_vals <- all_vals[is.finite(all_vals)]
  global_max  <- if (length(finite_vals) > 0) max(abs(finite_vals)) else 1
  if (global_max < .Machine$double.eps) global_max <- 1

  col_fun <- circlize::colorRamp2(c(-global_max, 0, global_max), scale_colors)

  # ----------------------------
  # 8. Build heatmaps
  # ----------------------------
  shared_anno_colors <- list(Group1 = group_colors, Group2 = group_colors)
  heatmap_list       <- vector("list", length(mat_list))

  for (i in seq_along(mat_list)) {

    mat     <- mat_list[[i]]
    sig_mat <- sig_list[[i]]
    g1      <- group1_list[[i]]
    g2      <- group2_list[[i]]

    ht <- ComplexHeatmap::Heatmap(
      mat,
      name             = "Signed\n-log10(BH)",
      col              = col_fun,
      na_col           = na_color,
      cluster_rows     = FALSE,
      cluster_columns  = FALSE,
      rect_gp          = grid::gpar(col = "black"),
      column_title     = "Adjusted p-value of Wilcox and GLMMs",
      column_title_gp  = grid::gpar(fontsize = 12, fontface = "bold"),

      left_annotation = ComplexHeatmap::rowAnnotation(
        Group1 = rep(g1, nrow(mat)),
        Group2 = rep(g2, nrow(mat)),
        col    = shared_anno_colors,
        annotation_legend_param = list(
          Group1 = list(title = "Group 1"),
          Group2 = list(title = "Group 2")
        )
      ),

      cell_fun = function(j, i, x, y, width, height, fill) {
        grid::grid.text(sig_mat[i, j], x, y, gp = grid::gpar(fontsize = 8))
      }
    )

    heatmap_list[[i]] <- ht
  }

  # ----------------------------
  # 9. Stack & draw
  # ----------------------------
  ht_final <- Reduce(`%v%`, heatmap_list)

  if (draw_heatmap) ComplexHeatmap::draw(ht_final, merge_legend = TRUE)

  invisible(ht_final)
}