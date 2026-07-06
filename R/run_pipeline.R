#' Run full spatial model benchmark pipeline
#' @export
run_spatial_benchmark <- function(
  data_dir,
  output_dir,
  params,
  glmm_models=list(),
  n_shuffle=100,
  run_model_qc_flag=TRUE,
  generate_report=TRUE
){
  set.seed(123)
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

  data <- load_spatial_data(data_dir,params$FOV_col)
  data <- prepare_counts_dens(data,params)
  cols <- get_analysis_columns(data,params)


  wilcox_res <- testWilcox(data, "Group",cols$dens_cols,paired=F)
  
  glmm_results <- list()
  for(name in names(glmm_models)){
    glmm_results[[name]] <- run_glmm(
      data,cols$count_cols,glmm_models[[name]]
    )
  }

  # QC
  if(run_model_qc_flag){
    for(name in names(glmm_results)){
      run_model_qc(glmm_results[[name]],
                   cols$count_cols,
                   file.path(output_dir,paste0("QC_",name)),
      model_spec = glmm_models[[name]])
    }
  }
# if (run_model_qc_flag) {
#   library(DHARMa)
#   library(purrr)
#   library(dplyr)
  
#   for (name in names(glmm_results)) {
    
#     # 1. Isolate the specific results and model specs for this distribution
#     current_results <- glmm_results[[name]]
#     current_spec    <- glmm_models[[name]]
    
#     # 2. Inject the formula and family metadata (so modelQC can use them for subtitles)
#     if (!is.null(current_spec)) {
#       current_results <- current_results %>%
#         mutate(
#           model_formula = current_spec$formula,
#           model_family  = current_spec$family
#         )
#     }
    
#     # 3. Build the subtitle column configuration list
#     subtitle_cols <- c("model_family", "model_formula")
#     if (all(c("group1", "group2") %in% colnames(current_results))) {
#       subtitle_cols <- c(subtitle_cols, "group1", "group2")
#     }
    
#     # 4. Manually run the DHARMa simulations across your count columns
#     # This step is mandatory before calling modelQC
#     simoutput_df <- current_results %>%
#       mutate(
#         across(
#           all_of(cols$count_cols),
#           ~ purrr::map(.x, function(model) {
#             tryCatch(simulateResiduals(model), error = function(e) NULL)
#           })
#         )
#       )
    
#     # 5. Set up the output directory and open the PDF graphic device
#     sub_output_dir <- file.path(output_dir, paste0("QC_", name))
#     dir.create(sub_output_dir, recursive = TRUE, showWarnings = FALSE)
    
#     pdf(file.path(sub_output_dir, "modelQC_plots.pdf"), width = 10, height = 8)
    
#     # 6. Execute the modelQC engine to draw the plots into the open PDF
#     modelQC(
#       current_results,
#       cols$count_cols,
#       simoutput_df = simoutput_df,
#       QQplots = TRUE,
#       AIC_BIC = TRUE,
#       plot_subtitle_cols = subtitle_cols
#     )
    
#     # 7. CRITICAL: Close the PDF device for this loop iteration
#     dev.off()
#   }
# }
#  res_list <- c(list(wilcox=wilcox_res),glmm_results)

  wilcox_list <- list(wilcox = wilcox_res)
   pvals_wilcox <- process_pvalues(wilcox_list, cols$dens_cols)
    pvals_wilcox$cell_type=gsub(params$colWilcox,'',pvals_wilcox$cell_type)
   
  glmm_list <- glmm_results  
   pvals_glmm <- process_pvalues(glmm_list, cols$count_cols)
pvals_glmm$cell_type=gsub(params$colGLMM,'',pvals_glmm$cell_type)
 
   pvals <- dplyr::full_join(
    pvals_wilcox,
    pvals_glmm,
    by = c("cell_type", "group1", "group2")
    )

 head(pvals)
  lfc <- compute_lfc(data,colWilcox=params$colWilcox,colGLMM=params$colGLMM)

  shuffle_res <- shuffleData(data,"Group","Sample_id",n_shuffle)

  saveRDS(pvals,file.path(output_dir,"pvalues.rds"))
  write.csv(pvals,file.path(output_dir,"pvalues.csv"))
  if(generate_report){
    render_report(
  data=data,
  results=pvals,
  lfc=lfc,
  shuffle=shuffle_res,
  glmm_models=glmm_models,
  glmm_results=glmm_results,   # ? ADD THIS
  output_dir=output_dir,
  params=  params,
    count_cols = cols$count_cols,   # ? ADD THIS
  dens_cols  = cols$dens_cols     # ? ADD THIS
)
  }

  list(pvalues=pvals,lfc=lfc)
}