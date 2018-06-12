#' Model KTS function
#'
#' This function makes predictions with the Kampala Trauma Score (KTS) model.
#' @param study_data The study data frame. No default.
#' @export
generate.model.predictions <- function(



                                       )
{
preds <- lapply(setNames(models_funcs$modelling_names, nm = model_names),
                function(func_name)
                {
                    fun <- get(func_name)
                    fun(study_data)
                }
                )
outcome <- study_data$s30d; levels(outcome) <- c(0,1)
## Bin model predictions
binned_preds <- lapply(setNames(model_names, nm = model_names),
                       function(model_name) bin.models(preds[[model_name]],
                                                       outcome,
                                                       models_funcs$by_seqs[[model_name]]))
## Convert to numeric preds
num_preds <- lapply(binned_preds,
                    function(pred) {
                        levels(pred) <- c("1","2","3","4")
                        as.numeric(pred)
                    }
                    )



}
