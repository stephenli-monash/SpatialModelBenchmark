#' create html reports
#' @export

render_report <- function(data, results, lfc, shuffle,glmm_models, glmm_results,
                          output_dir, params, count_cols, dens_cols){

 template_path <- "/fs04/scratch2/tc48/SPIAT/SpatialModelBenchmark/inst/rmarkdown/templates/benchmark/template.Rmd" 
  
rmarkdown::render(
  input = template_path,
  output_file = file.path(output_dir, "report.html"),
  params = list(
    data = data,
    results = results,
    lfc = lfc,
    shuffle = shuffle,
    glmm_models=glmm_models,
    glmm_results = glmm_results,
    count_cols = count_cols,
    dens_cols = dens_cols,
    user_params = params
  ),
  envir = new.env(parent = globalenv())    # ? CRITICAL FIX
  )
}
