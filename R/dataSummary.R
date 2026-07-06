#' @keywords internal 
#' NULL 
dataSummary <- function(data,
                        test_column,
                        id_col,
                        sort_by_id = FALSE,
                        draw_heatmap = FALSE) {
  
  require(ComplexHeatmap)
  require(tidyverse)
  
  if (is.grouped_df(data)) {
    warning("`data` has been ungrouped\n")
    data <- ungroup(data)
  }
  
  groups_present <- data[[test_column]] |> unique() |> sort()
  group_colors <- hcl.colors(length(groups_present), palette = "viridis")
  names(group_colors) <- groups_present

  comparisons <- groups_present |> combn(2, simplify = TRUE)
  
  df <- data |> 
    select(all_of(c(test_column, id_col))) |>
    group_by(across(all_of(c(test_column, id_col)))) |>
    summarise(.n = n(), .groups = "drop")
  
  df_wider <- df |> 
    pivot_wider(names_from = {{test_column}}, values_from = .n)
  
  if (sort_by_id) {
    df_wider <- df_wider |> arrange(across(all_of(id_col), ~.x))
  } else {
    df_wider <- df_wider |> arrange(across(all_of(groups_present), ~desc(.x)))
  }

  # count total number of groups present for each sample
  df_wider$total_num_groups <- df_wider |>
                               select(all_of(groups_present)) |>
                               mutate(across(all_of(groups_present), 
                                             ~ if_else(is.na(.x), 0, 1))) |>
                               rowSums()
  # return(df_wider)
  mat_n <- df_wider |> select(all_of(groups_present)) |> as.matrix()

  
  dimnames(mat_n) <- list(df_wider[[id_col]], groups_present)
  mat_groups <- df_wider |> 
    select(all_of(groups_present)) |>
    mutate(across(all_of(groups_present), ~ if_else(is.na(.x), "NA", cur_column()))) |>
    as.matrix()
  dimnames(mat_groups) <- list(df_wider[[id_col]], groups_present)


  ht <- Heatmap(mat_groups,
                name = test_column,
                
                cluster_rows = FALSE,
                cluster_columns = FALSE,
                
                col = c(group_colors, `NA` = "grey"),
                na_col = "grey",
                rect_gp = gpar(col = "black", lw = 1),
                
                show_column_names = TRUE,
                column_names_side = "bottom",
                column_names_rot = 0,
                column_names_centered = TRUE,
                
                column_title = test_column,
                column_title_side = "bottom",
                column_title_gp = gpar(fontface = "bold"),
                
                row_names_side = "left",
                
                row_title = id_col,
                row_title_gp = gpar(fontface = "bold"),
                
                show_heatmap_legend = FALSE,
                
                cell_fun = function(j, i, x, y, w, h, col) {
                  if(!is.na(mat_n[i, j]) & max(mat_n, na.rm = TRUE) > 1) {
                    if(j==1) {
                      curr_col <- "white"
                    } else {
                      curr_col <- "black"
                    }
                    grid.text(mat_n[i, j],
                              x, y,
                              gp = gpar(lineheight = h*0.8 , col = curr_col)
                    )}}
                )

  num_unique_ha <- rowAnnotation(
    num_unique = anno_numeric(df_wider$total_num_groups,
                         bg_gp = gpar(fill = "#fb8500", col = "#fb8500")),
    show_annotation_name = FALSE)
  
  num_unique_lgd <- Legend(labels = c("No. unique"), legend_gp = gpar(fill = c("#fb8500"))) 
  
  group_lgd <- Legend(labels = c(groups_present, "NA"), 
                      title = test_column,
                      legend_gp = gpar(fill = c(group_colors, `NA` = "grey")), 
                      )

  if(draw_heatmap) {
    draw(attach_annotation(ht, num_unique_ha, side = "right"), 
         annotation_legend_list = list(group_lgd, num_unique_lgd), merge_legend = TRUE)
  }
  invisible(list(ht = ht, ha = num_unique_ha, legend_list = list(group_lgd = group_lgd, num_unique_lgd = num_unique_lgd)))
}