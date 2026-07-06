#' @keywords internal 
#' @export
sigcountHeatmap <- function(sigcount_df,
                            n,
                            cols = NULL,
                            group_cols = c("group1", "group2"),
                            row_name_col = NULL,
                            group_color_palette = "viridis",
                            scale_colors = c("white","navy"), # "#154360"
                            na_color = "red",
                            mode = "number",
                            draw_heatmap = FALSE,
                            column_names_side = "top",
                            column_names_centered = TRUE,
                            column_names_rot = 0,
                            show_row_names = FALSE,
                            row_split = if(is.null(row_name_col)) {NULL} else {sigcount_df[[row_name_col]]},
                            row_title_rot = 0,
                            ...) {
  # NOTES:
  #   - add logical argument for stripping ".sig_counts"?
  #   - make more robust by adding final else statements to if-else blocks
  #   - what happens if user supplys argument to ... that is already used in this func?
   
  require(ComplexHeatmap)
  require(tidyverse)
  
  # checks
  if (is.null(cols)) {
    # get any cols that end with .sig_counts
    cols <- sigcount_df |> select(ends_with(".sig_counts")) |> names()
    if (is_empty(cols)) {
      stop("No columns ending with '.sig_counts' found. To use other columns specify them with `cols`")
    }
    
    counts_only <- sigcount_df |> 
                   select(all_of(cols)) |> 
                   rename_with(\(x) gsub(".sig_counts", "", x)) # strip ".sig_counts" from column names
  } else {
    cols_not_found <- cols[!(cols %in% names(sigcount_df))]
    if (!is_empty(cols_not_found)) {
      stop("Column(s) from `cols` not found in `sigcount_df`: ", paste0("`", cols_not_found, "`", collapse = ", "))
    }
    counts_only <- sigcount_df |> select(all_of(cols))
  }
  
  groups_not_found <- group_cols[!(group_cols %in% names(sigcount_df))]
  if (!is_empty(groups_not_found)) {
    stop("Column(s) from `group_cols` not found in `sigcount_df`: ", paste0("`", groups_not_found, "`", collapse = ", "))
  }
  
  if (length(scale_colors) != 2) {
    stop("`scale_colors` must be a vector of length [2]")
  }
  
  if (mode == "number") {
    mat <- counts_only |> as.matrix()
    
    col_fun <- colorRamp2(c(0, n), scale_colors)
    sigcount_lgd <- Legend(title = "Significant", c(0, round(n/2), n), col_fun = col_fun)
    
  } else if(mode == "proportion") {
    mat <- counts_only |> as.matrix()
    mat <- mat/n
    mat <- round(mat, 4)
    sigcount_lgd <- Legend(title = "Significant", c(0, 0.5, 1), col_fun = col_fun)
    
    col_fun <- colorRamp2(c(0, 1), scale_colors)
  } else if(mode == "scale_to_max") {
    mat <- counts_only |> as.matrix()
    col_fun <- colorRamp2(c(0, max(mat)), scale_colors)
    sigcount_lgd <- Legend(title = "Significant", c(0, round(max(mat)/2), max(mat)), col_fun = col_fun)
  } else {
    stop("Invalid value passed to `mode`: ", paste0(mode, collapse = " "), "\nMust be one of c('number', 'proportion')")
  }

  # set up comparison annotation
  comps_mat <- sigcount_df |> select(all_of(group_cols)) |> as.matrix()
  comps_elements <- comps_mat |> as.vector() |> unique() |> sort()
  comparison_colors <- hcl.colors(length(comps_elements), palette = group_color_palette)
  names(comparison_colors) <- comps_elements
  
  comps_ha <- rowAnnotation(Group = comps_mat, 
                            col = list(Group = comparison_colors),
                            show_annotation_name = FALSE)
  
  if (!is.null(row_name_col)) {
    rownames(mat) <- sigcount_df[[row_name_col]]
    rownames(comps_mat) <- sigcount_df[[row_name_col]]
  }
  
  ht <- Heatmap(mat,
                
                cluster_rows = FALSE,
                cluster_columns = FALSE,
                
                col = col_fun,
                na_col = na_color,
                rect_gp = gpar(col = "black", lw = 1),
                
                
                cell_fun = function(j, i, x, y, w, h, col) { # add count to each grid
                  grid.text(mat[i, j], x, y) },
                
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
         annotation_legend_list = list(sigcount_lgd), merge_legend = TRUE)
  }
  invisible(list(ht = ht, ha = comps_ha, legend_list = list(sigcount_lgd = sigcount_lgd)))
}