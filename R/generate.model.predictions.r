#' Generate model predictions function
#'
#' This function bins model predictions and converts them from character labels to numeric labels.
#' @param study_data The study data frame. No default
#' @param model_names Character vector of names of models. Defaults to c("RTS","GAP","KTS","gerdin")
#' @param log Logical. If TRUE progress is logged in logfile. Defaults to FALSE.
#' @param boot Logical. Affects only what is printed to logfile. If TRUE prepped_sample is assumed to be a bootstrap sample. Defaults to FALSE.
#' @param write_to_disk Logical. If TRUE the prediction data is saved as RDS to disk. Defaults to FALSE.
#' @param clean_start Logical. If TRUE the predictions directory and all files in it are removed before saving new stuff there. Defaults to FALSE.
#' @param gridsearch_parallel Logical. Passed to bin.models (which, in turn, passes to SupaLarnas gridsearch.breaks). If TRUE the gridsearch is performed in parallel. Defaults to FALSE.
#' @export
generate.model.predictions <- function(
                                       study_data,
                                       model_names = c("RTS",
                                                       "GAP",
                                                       "KTS",
                                                       "gerdin"),
                                       log = FALSE,
                                       boot = FALSE,
                                       write_to_disk = FALSE,
                                       clean_start = FALSE,
                                       gridsearch_parallel = TRUE
                                       )
{
    ## Define dir_name for write_to_disk
    dir_name <- "predictions"
    ## List modelling functions names and the spacing used in grid search
    preds_list <- list(modelling_names = unlist(lapply(model_names,
                                                       function(name) paste0("model.",
                                                                             name))),
                       by_seqs = setNames(as.list(c(0.5, 1, 1, 0.01)), model_names))
    ## Generate predictions with models
    preds <- lapply(setNames(preds_list$modelling_names, nm = model_names),
                    function(func_name)
                    {
                        fun <- get(func_name) # Get function from string
                        fun(study_data)       # Make predictions on study_data
                    }
                    )
    ## Extract outcome from study_data; Then, coerce to numeric
    outcome <- study_data$s30d; levels(outcome) <- c("0","1")
    outcome <- as.numeric(as.character(outcome))
    ## Bin model predictions
    binned_preds <- lapply(setNames(model_names, nm = model_names),
                           function(model_name) bin.models(preds[[model_name]],
                                                           outcome,
                                                           preds_list$by_seqs[[model_name]],
                                                           gridsearch_parallel = gridsearch_parallel))
    ## Convert to numeric preds
    num_preds <- lapply(binned_preds,
                        function(pred) {
                            levels(pred) <- c("1","2","3","4")
                            as.numeric(as.character(pred))
                        }
                        )
    ## Bind outcome and tc to outcome
    num_preds$outcome <- outcome
    num_preds$tc <- as.numeric(study_data$tc)
    ## Define pred_data
    pred_data <- list(continous_predictions = preds,
                      cut_predictions = num_preds)
    ## Define timestamp
    timestamp <- Sys.time()
    file_name <- ""
    ## Write each prediction to disk
    if (write_to_disk) {
        if (!dir.exists(dir_name)) dir.create(dir_name)
        filenum <- as.character(round(as.numeric(timestamp)*1000))
        file_name_ext <- "main"
        if (boot) file_name_ext <- "boot"
        file_name <- paste0(dir_name, "/model_predictions_", file_name_ext, "_", filenum, ".rds")
        saveRDS(pred_data, file_name)
        file_name <- paste0("saved in ", file_name, " ")
    }
    ## Log
    if (log) {
        analysis_name <- "Main"
        if (boot) analysis_name <- "Bootstrap"
        logline <- paste0(analysis_name, " analysis ", file_name, "completed on ", timestamp)
        append <- ifelse(clean_start, FALSE, TRUE)
        write(logline, "logfile", append = append)
    }
    return (pred_data)
}

