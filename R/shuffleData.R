#' @keywords internal 
#' NULL 
shuffleData <- function(data, test_column, id_col, n, shuffle_prefix = "shuffled.") {
  
  require(tidyverse)
  
  # checks
  if (!(test_column %in% names(data))) {
    stop(sprintf("test_column `%s` not found in data.", test_column))
  }
  
  if (!is.null(id_col)) {
    if (!(id_col %in% names(data))) {
      stop(sprintf("id_col `%s` not found in data.", id_col))
    }
  }
  
  if (n <= 0) {
    stop("n must be greater than zero.")
  }
  
  # note: numbers are zero padded after the shuffle prefix i.e. shuffled.001
  n_dig <- nchar(as.character(n))
    
  N <- nrow(data)
  
  shuffled_df <- data
  shuffle_names <- c()
  for (i in 1:n){
    shuffled_col_name = sprintf("%s%s", shuffle_prefix, str_pad(as.character(i), width = n_dig, pad = "0", side = "left"))
    shuffle_names[i] <- shuffled_col_name
    
    sample_vec <- sample(1:N, N)
    shuffled_df[[shuffled_col_name]] <- data[[test_column]][sample_vec]
    if (!is.null(id_col)) {
      shuffled_df[[paste0(shuffled_col_name, "_ID")]] <- data[[id_col]][sample_vec]
    }
  }
  
  return(list(shuffled_df = shuffled_df, shuffle_names = shuffle_names))
  
}