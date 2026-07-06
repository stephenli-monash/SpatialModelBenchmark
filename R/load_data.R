#' Load and merge CSV files
#' @export
load_spatial_data <- function(data_dir, fov_col) { 
files <- list.files(data_dir, pattern = "*.csv", full.names = 
TRUE) 

 if (length(files) == 0) stop("No CSV files found") 

data <- files |> 
    purrr::map(read.csv) |> 
    purrr::reduce(dplyr::full_join, by = fov_col) 
    colnames(data) <- gsub("[.]x", "", colnames(data)) 
    data 
    }