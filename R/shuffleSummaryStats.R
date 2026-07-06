#' @keywords internal 
#' NULL 
shuffleSummaryStats <- function(shuffled_data, 
                                shuffle_cols, 
                                cols = NULL) {
  # shuffle_cols: names of columns containing groups
  # cols: numeric columns with data
  
  require(tidyverse)
  require(moments)
  
  if (is.grouped_df(shuffled_data)) {
    warning("`shuffled_data` has been ungrouped\n")
    shuffled_data <- ungroup(shuffled_data)
  }
  
  
  if (is.null(cols)) {
    cols <- shuffled_data |> select(where(is.numeric)) |> names()
  }
  
  # calculate summary stats
  sum_stats <- shuffled_data |>
    pivot_longer(all_of(shuffle_cols), names_to = "shuffleID", values_to = "shuffle_group") |> 
    pivot_longer(all_of(cols), names_to = "type", values_to = ".value") |> 
    group_by(type, shuffleID, shuffle_group) |>
    summarise(
      min = min(.value, na.rm = TRUE),
      max = max(.value, na.rm = TRUE),
      mean = mean(.value, na.rm = TRUE),
      median = median(.value, na.rm = TRUE),
      sd = sd(.value, na.rm = TRUE),
      var = var(.value, na.rm = TRUE),
      skewness = skewness(.value, na.rm = TRUE),
      kurtosis = kurtosis(.value, na.rm = TRUE),
      cv = sd / mean * 100,
      .groups = "drop"
    )
  
  return(sum_stats)
}