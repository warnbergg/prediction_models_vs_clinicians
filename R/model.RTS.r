#' Model RTS
#'
#' This function makes predictions with the RTS model.
#' @param study_data The study data frame. No default.
#' @export
model.RTS <- function(
                      study_data
                      )
{
    ## Define model_variables
    model_variables <- c("sbp",
                         "gcs",
                         "rr")
    ## Define cut points
    cut_points <- list(sbp = c(0, 1, 49, 75, 89, Inf),
                       gcs = c(3,4,5,8,12,Inf),
                       rr = c(0,1,5,9,29, Inf))
    ## Define scores of variabels
    scores <- list(sbp = c("0", "1", "2", "3", "4"),
                   gcs = c("0", "1", "2", "3", "4"),
                   rr = c("0", "1", "2", "4", "3"))
    ## Define RTS coefficients
    RTS_coefficients <- c(0.9368,
                          0.7326,
                          0.2908)
    ## Use bin.model.variables to group model variables into scores
    binned_variables <- lapply(setNames(model_variables, nm = model_variables),
                               function(col){
                                   bin.model.variables(study_data,
                                                       model_variables,
                                                       cut_points,
                                                       scores)[, col]
                               }
                               )
    ## Apply RTS coefficients to variables and sum rows to generate predictions.
    ## Then, invert.
    predictions <- rowSums(mapply('*',
                                  binned_variables,
                                  RTS_coefficients))

    return (predictions)
}
