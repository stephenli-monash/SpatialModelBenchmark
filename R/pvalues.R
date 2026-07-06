#' Combine and adjust p-values
#' @export
process_pvalues <- function(res_list, cols){

  if (is.null(names(res_list))) {
    stop("res_list must be a named list")
  }

  res_list <- Map(function(res, name){

    df <- getPvalues(res, cols) |>
      tidyr::pivot_longer(
        cols = cols,
        names_to = "cell_type",
        values_to = "pval"
      )

    # ? rename pval column by model
    df <- df |>
      dplyr::rename(!!paste0(name, "_pval") := pval)

    return(df)

  }, res_list, names(res_list))

  combined <- purrr::reduce(
    res_list,
    dplyr::left_join,
    by = c("cell_type","group1","group2")
  )

  # ? adjust all p-values safely
  combined <- combined |>
    dplyr::mutate(
      across(
        ends_with("_pval"),
        ~ p.adjust(.x, "BH"),
        .names = "{.col}_adj"
      )
    )

  return(combined)
}