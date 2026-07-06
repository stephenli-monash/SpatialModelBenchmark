#' extract column for modelling
#' @export
 get_analysis_columns <- function(data, params) { 
      
       list(          count_cols = grep(params$colGLMM, colnames(data), value = TRUE), 
         dens_cols  = grep(params$colWilcox, colnames(data), value = TRUE) 
       ) 
     }