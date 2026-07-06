#' Prepare analysis dataset
#' @export
   prepare_counts_dens <- function(data, params) {     
     
        data |> 
          dplyr::mutate( 
            Area.mm2 = as.numeric(.data[[params$Total_ROI_Area]]), 
            Total_Cell_Number = .data[[params$Total_Cell_Number]] 
          ) |> 
          dplyr::select(
             patient_id = all_of(params$patient_col), 
             Group = all_of(params$test_column), 
             Sample_id = all_of(params$FOV_col), 
             Area.mm2, 
             Total_Cell_Number, 
             dplyr::ends_with(params$colGLMM), 
             dplyr::ends_with(params$colWilcox) 
           ) |> 
           dplyr::mutate(Group = as.factor(Group)) 
       } 
       
