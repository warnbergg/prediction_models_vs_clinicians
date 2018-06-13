#' Generate model predictions function
#'
#' This function bins model predictions and converts them from character labels to numeric labels.
#' @param study_data The study data frame. No default
#' @param model_names Character vector of names of models. Defaults to c("RTS","GAP","KTS","gerdin")
#' @export
generate.model.predictions <- function(
                                       study_data,
                                       model_names = c("RTS",
                                                       "GAP",
                                                       "KTS",
                                                       "gerdin")
                                       )
{
    ## If predictions have been run, load directory
    if (file.exists("./predictions.rds")){
        predictions <- readRDS("./predictions.rds")

        return (predictions)
    } else {

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
                                                               preds_list$by_seqs[[model_name]]))
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
        ## Save to disk
        saveRDS(num_preds, "./predictions.rds")

        return (num_preds)
    }
}
