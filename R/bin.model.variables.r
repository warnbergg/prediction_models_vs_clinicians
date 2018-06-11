#' Bin model variables into scores
#'
#' This function bins model variables according to cutpoints.
#' @param study_data The study data frame. No default.
#' @param model_variables The variables to be included in the model. As character vector. No default.
#' @param cut_points The cut points used to bin the model variables. As list of numeric vectors. No default.
#' @param levels The scores to be assigned to each bin. As list of character vectors. No default.
#' @export
bin.model.variables <- function(
                               study_data,
                               model_variables,
                               cut_points,
                               levels)
{
    ## Subset variables for model
    model_df <- study_data[, model_variables]
    ## Bin variables according to cut points
    binned_variables <- sapply(model_variables,
                               function (var){
                                   cut_var <- cut(model_df[,var],
                                                  cut_points[[var]],
                                                  labels = levels[[var]],
                                                  include.lowest = TRUE)
                                   num_var <- as.numeric(cut_var)
                                   return (num_var)
                               }
                               )
    return (binned_variables)
}
