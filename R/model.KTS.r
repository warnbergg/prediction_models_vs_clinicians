#' Model KTS function
#'
#' This function makes predictions with the Kampala Trauma Score (KTS) model.
#' @param study_data The study data frame. No default.
#' @export
model.KTS <- function(
                      study_data
                      )
{
    ## Define variables to be included in model. Same with
    ## nsi, change value to 3,2,1. Age is excluded
    ## and binded later with duplicate factor labels.
    model_variables <- c("sbp",
                         "rr")
    ## Define cut points for variables; bind avpu, and change values
    ## to 1,2,3,4 later. Same with nsi, change value to 3,2,1
    cut_points <- list(sbp = c(0, 1, 49, 89, Inf),
                       rr = c(0,10, 29, Inf))
    ## Define scores from bins
    scores <- list(sbp = c("1","2","3","4"),
                   rr = c("1","3","2"))
    ## Get age from study_data
    age <- study_data$age
    ## Bin age
    binned_age <- as.numeric(cut(age,
                                 breaks = c(0,5,55,Inf),
                                 include.lowest = TRUE))
    ## Asign labels to binned variables
    age_var <- c(1,2,1)[binned_age]
    ## Change levels of nsi to 3,2,1 to correspond to score
    ## and coerce to numeric vector
    levels(study_data$nsi) <- c("3", "2", "1")
    study_data$nsi <- as.numeric(as.character(study_data$nsi))
    ## Bin model variables
    binned_variables <- bin.model.variables(study_data,
                                            model_variables,
                                            cut_points,
                                            scores)
    ## Sum binned variables to generate score
    kts_predictions <- 1/rowSums(cbind(binned_variables,
                                       binned_age,
                                       study_data$avpu,
                                       study_data$nsi))

    return(kts_predictions)
}
