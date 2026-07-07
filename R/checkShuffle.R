#' Check Data Shuffle Distribution
#'
#' Visualises how data fields are distributed across shuffle groups 
#' by generating a breakdown bar chart and frequency histograms.
#'
#' @param shuffled_data A data frame or tibble containing the shuffled data.
#' @param id_cols A character vector of column names used to uniquely identify rows.
#' @param shuffle_cols A character vector of column names representing the shuffle groups.
#'
#' @return A ggplot object combining a stacked bar chart and faceted histograms.
#' @importFrom dplyr select all_of ungroup group_by count arrange
#' @importFrom tidyr unite pivot_longer
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal theme scale_fill_manual geom_histogram facet_wrap
#' @importFrom ggpubr ggarrange
#' @importFrom circlize colorRamp2
#' @export
checkShuffle <- function(shuffled_data, id_cols, shuffle_cols) {
  
  if (inherits(shuffled_data, "grouped_df")) {
    warning("`shuffled_data` has been ungrouped\n")
    shuffled_data <- dplyr::ungroup(shuffled_data)
  }
  
  groups <- shuffled_data |> 
            dplyr::select(dplyr::all_of(c(shuffle_cols))) |> 
            as.matrix() |> 
            as.vector() |> 
            unique() |> 
            sort()
  
  shuffled_data <- shuffled_data |> tidyr::unite(id_col, dplyr::all_of(id_cols), sep = ("-"))
  shuffled_data$id_col <- factor(shuffled_data$id_col, levels = shuffled_data$id_col)
  
  group_count_df <- 
    shuffled_data |> 
    dplyr::select(dplyr::all_of(c("id_col", shuffle_cols))) |> 
    tidyr::pivot_longer(cols = dplyr::all_of(shuffle_cols), names_to = "shuffle_id", values_to = "group") |> 
    dplyr::group_by(id_col) |> 
    dplyr::count(group) |> 
    dplyr::arrange(id_col) |>
    dplyr::ungroup()
  
  col_fun <- circlize::colorRamp2(c(1, length(groups)), c("#A9CCE3","#154360"))
  p1 <- ggplot2::ggplot(group_count_df, ggplot2::aes(x=n, y=id_col, fill= group)) +
    ggplot2::geom_bar(stat="identity") + 
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal") +
    ggplot2::scale_fill_manual(values = col_fun(seq(1:length(groups))))
  
  p2 <- ggplot2::ggplot(group_count_df, ggplot2::aes(x=n, fill=group)) +
    ggplot2::geom_histogram(bins = 10, color="white") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal") +
    ggplot2::scale_fill_manual(values = col_fun(seq(1:length(groups)))) +
    ggplot2::facet_wrap("group", nrow = 1)
  
  return(ggpubr::ggarrange(p1, p2, ncol = 1, common.legend = TRUE, heights = c(2,1)))
}
