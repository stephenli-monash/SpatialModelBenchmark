#' @keywords internal 
#' NULL 
pvalueHeatmap <- function(pvalue_df, 
                          cols = NULL, 
                          group_cols = c("group1", "group2"),
                          row_name_col = NULL,
                          sig_levels = c(0.05, 0.01, 0.001),
                          group_color_palette = "viridis",
                          scale_colors = c("white","darkgreen"),
                          na_color = "red",
                          draw_heatmap = FALSE,
                          column_names_side = "top",
                          column_names_centered = TRUE,
                          column_names_rot = 0,
                          show_row_names = FALSE,
                          row_split = if(is.null(row_name_col)) {NULL} else {pvalue_df[[row_name_col]]},
                          row_title_rot = 0,
                          ...) {
  # NOTES:
  #   - what happens if user supplys argument to ... that is already used in this func?
  #   - added sig marks: 
  #     - https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#simple-annotation-as-an-annotation-function
  #     - https://jokergoo.github.io/ComplexHeatmap-reference/book/legends.html#a-list-of-legends
  
  require(ComplexHeatmap)
  require(tidyverse)
  
  # checks
  if (is.null(cols)) {
    cols <- pvalue_df |> select(where(is.numeric)) |> names()
  } else {
    cols_not_found <- cols[!(cols %in% names(pvalue_df))]
    if (!is_empty(cols_not_found)) {
      stop("Column(s) from `cols` not found in `pvalue_df`: ", paste0("`", cols_not_found, "`", collapse = ", "))
    }
  }
  
  groups_not_found <- group_cols[!(group_cols %in% names(pvalue_df))]
  if (!is_empty(groups_not_found)) {
    stop("Column(s) from `group_cols` not found in `pvalue_df`: ", paste0("`", groups_not_found, "`", collapse = ", "))
  }
  
  if (length(scale_colors) != 2) {
    stop("`scale_colors` must be a vector of length [2]")
  }
  
  # set up comparison annotation
  comps_mat <- pvalue_df |> select(all_of(group_cols)) |> as.matrix()
  comps_elements <- comps_mat |> as.vector() |> unique() |> sort()
  comparison_colors <- hcl.colors(length(comps_elements), palette = group_color_palette)
  names(comparison_colors) <- comps_elements
  
  comps_ha <- rowAnnotation(Group = comps_mat, 
                            col = list(Group = comparison_colors),
                            show_annotation_name = FALSE)
  
  # OLD from when comparison colors was a vector of colors
  # if (is.null(comparison_colors)) {
  #   comparison_colors <- hcl.colors(length(comps_elements), palette = "viridis")
  #   names(comparison_colors) <- comps_elements
  # } else if (length(comparison_colors) != length(comps_elements)) {
  #   stop("`comparison_colors` [", 
  #        length(comparison_colors),
  #        "] must be the same length as number of unique comparison elements [",
  #        length(comps_elements), "]")
  # } else if (is.null(names(comparison_colors))) {
  #   names(comparison_colors) <- comps_elements
  # } else if (!all(comps_elements %in% names(comparison_colors))) {
  #   stop("Names of comparison_colors must be the same as groups present in group columns")
  # }
  
  # convert df of pvalues to matrix
  pvalues_mat <- pvalue_df |> select(all_of(cols)) |> as.matrix()
  pvalues_mat.log <- -log10(pvalues_mat)
  
  if (!is.null(row_name_col)) {
    rownames(pvalues_mat) <- pvalue_df[[row_name_col]]
    rownames(pvalues_mat.log) <- pvalue_df[[row_name_col]]
    rownames(comps_mat) <- pvalue_df[[row_name_col]]
  }
  
  # significance level markings
  sig_mat <- rep("", length(pvalues_mat)) |> matrix(nrow = nrow(pvalues_mat))
  pvalues_mat_no_NAs <- pvalues_mat
  pvalues_mat_no_NAs[is.na(pvalues_mat_no_NAs)] <- 1
  for (i in 1:length(sig_levels)) {
    sig_mat[pvalues_mat_no_NAs <= sig_levels[[i]]] <- paste0(rep("*", i), collapse = "")
  }
 

   
  # legends
  lgd_values <- 0:(max(5,ceiling(-log10(last(sig_levels)))))
  lgd_labels <- 10^-lgd_values |> formatC(format = "fg")
  
  col_fun <- colorRamp2(c(first(lgd_values), last(lgd_values)), scale_colors)
  
  pvalue_lgd <- Legend(title = "p-value", 
                       col_fun = col_fun,
                       at = lgd_values,
                       labels = lgd_labels)
  sig_lgd <- Legend(pch = map_chr(1:length(sig_levels), \(i) paste0(rep("*",i), collapse = "")),
                    type = "points",
                    labels = paste0("< ", formatC(sig_levels, format = "fg")))
  na_lgd <- Legend(labels = c("NA"), legend_gp = gpar(fill = na_color))

  ht <- Heatmap(
    pvalues_mat.log,

    cluster_rows = FALSE,
    cluster_columns = FALSE,

    col = col_fun,
    na_col = na_color,
    rect_gp = gpar(col = "black", lwd = 1),

    # add mark if significant
    cell_fun = function(j, i, x, y, width, height, fill) {
      # y <- y - 0.2*height # move asteriks down a little bit
      grid.text(sig_mat[i,j], x, y)
    },
    
    show_heatmap_legend = FALSE,
    
    # `Heatmap()` arguments with defaults
    column_names_side = column_names_side,
    column_names_centered = column_names_centered,
    column_names_rot = column_names_rot,
    
    show_row_names = show_row_names,
    row_split = row_split,
    row_title_rot = row_title_rot,
    
    # any other `Heatmap` args
    ...
  )
  if(draw_heatmap) {
    draw(attach_annotation(ht, comps_ha, side = "left"), 
         annotation_legend_list = list(pvalue_lgd, sig_lgd, na_lgd), 
         merge_legend = TRUE)
  }
  invisible(list(ht = ht, ha = comps_ha, legend_list = list(pvalue_lgd = pvalue_lgd, sig_lgd = sig_lgd, na_lgd = na_lgd)))
}