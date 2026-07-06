#' Run shuffle benchmarking
#' @export
run_shuffle <- function(data, params, n = 100) {

  shuffle_res <- shuffleData(
    data = data,
    id_col = "Sample_id",
    test_column = "Group",
    n = n
  )

  return(shuffle_res)
}
