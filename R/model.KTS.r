#' Model KTS function
#'
#' This function makes predictions with the Kampala Trauma Score (KTS) model.
#' @param study_data The study data frame. No default.
#' @export
model.KTS <- function(
                      study_data
                      )
{
    ## Define variables to be included in model. Bind avpu, and change values
    ## to 1,2,3,4 later. Same with nsi, change value to 3,2,1
    model_variables <- c("age",
                         "sbp",
                         "rr")
    ## Define cut points for variables; bind avpu, and change values
    ## to 1,2,3,4 later. Same with nsi, change value to 3,2,1
    cut_points <- list(age = c(0,5,55,Inf),
                       sbp = c(0, 1, 49, 89, Inf),
                       rr = c(0,10, 29, Inf))
    binned_variables <- bin.model.variables(study_data,
                                            model_variables,
                                            cut_points)
    ## Sum binned variables to generate score
    kts_predictions <- rowSums(cbind(binned_variables,
                                     study_data$avpu,
                                     study_data$nsi))

    return(kts_predictions)
}
