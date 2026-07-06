#' @keywords internal 
#' NULL 
checkShuffle <- function(shuffled_data, id_cols, shuffle_cols) {
  
  require(tidyverse)
  
  if (is.grouped_df(shuffled_data)) {
    warning("`shuffled_data` has been ungrouped\n")
    shuffled_data <- ungroup(shuffled_data)
  }
  
  groups <- shuffled_data |> 
            select(all_of(c( shuffle_cols))) |> 
            as.matrix() |> 
            as.vector() |> 
            unique() |> 
            sort()
  
  # add id column with concatenated names from id_cols
  shuffled_data <- shuffled_data |> unite(id_col, all_of(id_cols), sep = ("-"))
  shuffled_data$id_col <- factor(shuffled_data$id_col, levels = shuffled_data$id_col)

  # check ids are unique?
  
  # count groups for each id
  group_count_df <- 
    shuffled_data |> 
    select(all_of(c("id_col", shuffle_cols))) |> 
    pivot_longer(cols = all_of(shuffle_cols), names_to = "shuffle_id", values_to = "group") |> 
    group_by(id_col) |> 
    count(group) |> 
    arrange(id_col) |>
    ungroup()
  
  # plot 1 - bar graph for id_col
  # add title
  col_fun <- colorRamp2(c(1, length(groups)), c("#A9CCE3","#154360"))
  p1 <- ggplot(group_count_df, aes(x=n, y=id_col, fill= group)) +
    geom_bar(stat="identity") + 
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction = "horizontal") +
    scale_fill_manual(values = col_fun(seq(1:length(groups))))
  
  # plot 2 - histogram
  p2 <- ggplot(group_count_df, aes(x=n, fill=group)) +
    geom_histogram(bins = 10, color="white") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction = "horizontal") +
    scale_fill_manual(values = col_fun(seq(1:length(groups)))) +
    facet_wrap("group", nrow = 1)
  
  return(ggarrange(p1,p2, ncol = 1, common.legend = TRUE, heights = c(2,1)))
}