run_model_qc <- function(glmm_results, count_cols, output_dir = NULL, model_spec = NULL) {

  library(DHARMa)
  library(purrr)
  library(dplyr)

  if (is.null(glmm_results)) {
    message("No GLMM results provided for QC.")
    return(invisible(NULL))
  }

  # Add formula/family as columns so modelQC can use them as subtitles
  if (!is.null(model_spec)) {
    glmm_results <- glmm_results %>%
      mutate(
        model_formula = model_spec$formula,
        model_family  = model_spec$family
      )
  }

  # Build subtitle column list: formula/family + group comparison (if present)
  subtitle_cols <- character(0)
  if (!is.null(model_spec)) {
    subtitle_cols <- c(subtitle_cols, "model_family", "model_formula")
  }
  if (all(c("group1", "group2") %in% colnames(glmm_results))) {
    subtitle_cols <- c(subtitle_cols, "group1", "group2")
  }
  if (length(subtitle_cols) == 0) subtitle_cols <- NULL

  simoutput_df <- glmm_results %>%
    mutate(
      across(
        all_of(count_cols),
        ~ purrr::map(.x, function(model) {
          tryCatch(simulateResiduals(model), error = function(e) NULL)
        })
      )
    )

  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    pdf(file.path(output_dir, "modelQC_plots.pdf"), width = 10, height = 8)
  }

  modelQC(
    glmm_results,
    count_cols,
    simoutput_df = simoutput_df,
    QQplots = TRUE,
    AIC_BIC = TRUE,
    plot_subtitle_cols = subtitle_cols
  )

  if (!is.null(output_dir)) dev.off()

  invisible(NULL)
}