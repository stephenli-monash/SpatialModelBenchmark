#' @keywords internal 
#' NULL 
modelQC <- function(results_df, 
                    cols,
                    simoutput_df = NULL,
                    resid_tests = "all",
                    AIC_BIC = TRUE,
                    QQplots = FALSE,
                    plot_subtitle_cols = NULL,
                    testOutliers_type = "default",
                    testOutliers_nBoot = 100,
                    layout_mat = matrix(1:4, 2,2, byrow = TRUE)) {

  require(DHARMa)
  require(tidyverse)
  
  # Notes:
  #   - resid_tests:
  #       - "all"
  #       - "KS_test"
  #       - "disp_test"
  #       - "outliers_test"
  #       - "none"
  #   - simoutput_df has same columns except models have been replaced with DHARMa sim objects (from simulateResiduals)
  
  # checks
  if (is.grouped_df(results_df)) {
    warning("`results_df` has been ungrouped\n")
    data <- ungroup(results_df)
  }

  if ((!all(cols %in% names(results_df)))){
    not_found <- cols[!(cols %in% names(results_df))]
    stop(sprintf("Column(s) %s not found in results data-frame.", paste0("`", not_found,"`", collapse = ", ")))
  }
  if (QQplots) {  
    if ((!all(plot_subtitle_cols %in% names(results_df)))){
      not_found <- plot_subtitle_cols[!(plot_subtitle_cols %in% names(results_df))]
      stop(sprintf("Plot title column(s) %s not found in results data-frame.", paste0("`", not_found,"`", collapse = ", ")))
    } 
    if (any(plot_subtitle_cols %in% cols)) {
      stop("`plot_subtitle_cols` cannot also be in `cols`.")
    }
  }
  if (!resid_tests %in% c("all", "KS_test", "disp_test", "outliers_test", "none")){
    stop(sprintf("Invalaid `resid_tests` ['%s']. `resid_tests` must be one of", paste0("'", c("all", "KS_test", "disp_test", "outliers_test", "none"),"'", collapse = ", ")))
  }
  
  if (!is.null(simoutput_df)) {  
    if ((!all(cols %in% names(simoutput_df)))){
      not_found <- cols[!(cols %in% names(simoutput_df))]
      stop(sprintf("Column(s) %s not found in sim output data-frame.", paste0("`", not_found,"`", collapse = ", ")))
    }
    if (nrow(results_df) != nrow(simoutput_df)) {
      stop(sprintf("Number of rows in `simoutput_df` [%d] must equal number of rows in `results_df` [%d]", nrow(simoutput_df), nrow(results_df)))
    }
  }
  
  # use purrr::quietly to collect warnings (not errors)
  quiet_testUniformity <- quietly(testUniformity)
  quiet_testDispersion <- quietly(testDispersion)
  quiet_testOutliers <- quietly(testOutliers)
  
  out_list <- list()
  for (col in cols) {
    curr_df <- results_df |> select(!all_of(cols) | all_of(col)) |> rename(mdl = {{col}})
    if (!is.null(simoutput_df)) {
      curr_simoutput_df <- simoutput_df |> select(!all_of(cols) | {{col}}) |> rename(sim_output = {{col}})
      curr_df <- left_join(curr_df, curr_simoutput_df, by = intersect(names(curr_df), names(curr_simoutput_df)))
    } else {
      curr_df <- curr_df |> mutate(sim_output = map(mdl, \(x) simulateResiduals(x)))
    }
    if (resid_tests == "KS_test" | resid_tests == "all") {
      curr_df <- curr_df |> 
        mutate(KS_test = map(sim_output, \(x) quiet_testUniformity(x, plot = F)))
    }
    
    if (resid_tests == "disp_test" | resid_tests == "all") {
      curr_df <- curr_df |> 
        mutate(disp_test = map(sim_output, \(x) quiet_testDispersion(x, plot = F)))
    }
    
    if (resid_tests == "outliers_test" | resid_tests == "all") {
      curr_df <- curr_df |> 
        mutate(outliers_test = map(sim_output, \(x) quiet_testOutliers(x, type = testOutliers_type, nBoot = testOutliers_nBoot, plot = F)))
    }
    
    if (AIC_BIC) {
      curr_df <- curr_df |>  mutate(AIC = map_dbl(mdl, \(x) AIC(x)),
                                    BIC = map_dbl(mdl, \(x) BIC(x)))
    }
    
    out_list[[col]] <- curr_df
  }

  tmp <- out_list |> enframe(name = "col_name", value = ".value") |> unnest(cols = c(.value)) 
  out_df <- tmp |> mutate(across(ends_with("test"), ~map(.x, \(x) x[["result"]])))
  warn_df <- tmp |> select(!any_of(c("mdl", "sim_output", "AIC", "BIC"))) |> mutate(across(ends_with("test"), ~map(.x, \(x) x[["warnings"]])))
  msg_df <- tmp |> select(!any_of(c("mdl", "sim_output", "AIC", "BIC"))) |> mutate(across(ends_with("test"), ~map(.x, \(x) x[["messages"]])))
  
  # sort df by colname
  out_df <- arrange(out_df, across(col_name))
  warn_df <- arrange(warn_df, across(col_name))
  msg_df <- arrange(msg_df, across(col_name))
  
  if (resid_tests == "all") {
    test_cols <- c("KS_test", "disp_test", "outliers_test")
  } else {
    test_cols <- resid_tests
  }
  
  if (resid_tests != "none") {
    out_df <- out_df |>
      mutate(across(all_of(test_cols), 
                    ~ map_dbl(.x, \(tst) tst$p.value),
                    .names = "{gsub('_test', '.p', .col)}"))
    
    # extract & print warnings
    warn_df <- warn_df |>
      pivot_longer(cols = all_of(test_cols), names_to = "residual_test", values_to = "warning") |>
      mutate(empty = map_lgl(warning, is_empty)) |> 
      filter(!empty) |> 
      unnest(cols = c(warning)) |>
      select(!(empty| residual_test))
    
    if (nrow(warn_df) > 0) {    
      # format warning output
      # add header row
      names_row <- names(warn_df)
      names(names_row) <- names_row
      warn_df <- add_row(warn_df, as_tibble_row(names_row), .before = 1)
      warn_df <- warn_df |> 
        mutate(across(everything(),
                      ~ str_pad(.x, max(nchar(.x)), side = "right"))) |>
        unite(out_strs, everything(), sep = "  ")
      
      message(paste0(
        "\nWarnings from residual testing:\n  ",
        paste0(warn_df$out_strs, collapse = "\n  ")
      ))
    }
    
    # extract & print message
    msg_df <- msg_df |>
      pivot_longer(cols = all_of(test_cols), names_to = "residual_test", values_to = "Message") |>
      mutate(empty = map_lgl(Message, is_empty)) |> 
      filter(!empty) |> 
      unnest(cols = c(Message)) |>
      select(!(empty | residual_test))
    
    if (nrow(msg_df) > 0) {    
      # format warning output
      # add header row
      names_row <- names(msg_df)
      names(names_row) <- names_row
      msg_df <- add_row(msg_df, as_tibble_row(names_row), .before = 1)
      msg_df <- msg_df |> 
        mutate(across(!Message,
                      ~ str_pad(.x, max(nchar(.x))+2, side = "right")))
      
      tmp <- select(msg_df, !Message)
      w <- tmp[1,] |> mutate(across(everything(), nchar)) |> rowSums()
      
      msg_df <- msg_df |> 
        mutate(across(Message, ~gsub("[.] ", 
                                     paste0(str_pad(".\n", w+1, side = "right")),
                                     .x))) |>
        unite(out_strs, everything(), sep = "")
      
      # return(msg_df)
      message(paste0(
        "\nMessages from residual testing:\n",
        paste0(msg_df$out_strs, collapse = "\n")
      ))
    }
  }

  if(QQplots) {
    # construct subtitle string
    if (is.null(plot_subtitle_cols)) {
      subtitle_vec <- rep("", nrow(out_df))
    } else {
      tmp <- out_df |>
        select(all_of(plot_subtitle_cols)) |>
        unite(subtitle, all_of(plot_subtitle_cols), sep = " ")
      subtitle_vec <-  tmp$subtitle
    }
    
    layout(layout_mat)
    for (i in 1:nrow(out_df)) {
      main_txt <- paste0("DHARMa QQ plot residuals: ",
                         out_df$col_name[[i]],
                         "\n",
                         subtitle_vec[[i]])
      
      # could draw plots using `qqunif(residuals(sim_output), logscale = FALSE, col = "black")`
      suppressMessages(plotQQunif(out_df$sim_output[[i]], 
                                  main = main_txt,
                                  testUniformity = ifelse(resid_tests == "all" | resid_tests == "KS_test", T, F),
                                  testOutliers = ifelse(resid_tests == "all" | resid_tests == "outliers_test", T, F),
                                  testDispersion = ifelse(resid_tests == "all" | resid_tests == "disp_test", T, F)))
    }
  }
  
  # remove mdl column
  out_df <- select(out_df, !mdl)
  return(out_df)
}
