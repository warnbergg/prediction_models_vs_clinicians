#' Create the Gerdin et al. model function
#'
#' This function makes predictions with the Gerdin et al. model.
#' @param study_data The study data frame. No default.
#' @export
model.gerdin <- function(
                         study_data
                         )
{
    ## Define model variables; bind gcs later
    model_variables <- c("sbp",
                         "hr")
    ## Define knots for rcs used in the original article
    knots <- list(sbp = c(80, 110, 121, 147),
                  hr = c(70, 86, 92, 118))
    ## Define shrunk coefficients used in the original article
    shrunk_coefficients <- list(sbp = c(-0.0211,
                                        -0.0379,
                                        0.3386),
                                hr = c(-0.0030,
                                       -0.0157,
                                       0.2014),
                                gcs = -0.2048)
    ## Subset model variables from study_data
    model_df <- study_data[, model_variables]
    ## Model systolic blood pressure and heart rate with rcs
    modelled_variables <- lapply(setNames(model_variables, nm = model_variables),
                                 function(variable){
                                     ## Apply rcs function to create basis funcs
                                     basis_functions <- rms::rcs(model_df[, variable],
                                                                 parms = knots[[variable]])
                                     ## Change variable names to original article
                                     ## names of basis funcs
                                     colnames(basis_functions) <- unlist(
                                         lapply(1:3, function(num) paste0(variable, num)))
                                     ## Coerce to list
                                     l <- lapply(1:3,
                                                 function(i) basis_functions[, i])
                                     return(l)
                                     }
                                 )
    ## Apply shrunk coefficients to generate sbp1,2,3 and hr1,2,3
    ## according to original article; Then, coerce to list
    basis_w_coeff <- do.call(cbind,
                             lapply(model_variables,
                                    function(var)
                                        mapply('*',
                                               modelled_variables[[var]],
                                               shrunk_coefficients[[var]])
                                    ))
    ## Multiply gcs by shrunk coefficient, bind to other shrunked
    ## variables and add y-intercept
    ## add y-intercept
    y_model <- 2.2142 + rowSums(cbind(basis_w_coeff,
                                      study_data$gcs * shrunk_coefficients[["gcs"]]))
    ## Use y-model as parameter in simple logit formula to generate
    ## predictions
    predictions <- 1/(1 + exp(-y_model))

    return (predictions)
}
