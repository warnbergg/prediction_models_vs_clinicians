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
                                       0.2014))
    ## Subset model variables from study_data
    model_df <- study_data[, model_variables]
    ## Model systolic blood pressure and heart rate with rcs
    modelled_variables <- lapply(model_variables,
                                 function(variable)
                                     rcs(model_df[, variable],
                                         parms = knots[[variable]]
                                         ))
    ## Apply shrunk coefficients to generate sbp1,2,3 and hr1,2,3
    sbp_hr_w_coeff <- sapply(names(model_variables),
                             function(var)
                                 t(t(modelled_variables[[var]])*shrunk_coefficients[[var]])
                             )
    ## Multiply gcs by coefficient, bind to other shrunked variables and add y-intercept
    y_model <- 2.2142 + rowSums(cbind(sbp_hr_w_coeff,
                                      gcs * -0.2048))
    ## Use y-model as parameter in simple logit formula
    predictions <- 1/(1 + exp(-y_model))

    return (predictions)
    #Gerdinsbp <- rcs(Gerdin$sbp, parms = knots$sbp)
    #Gerdinhr <- rcs(Gerdin$hr, parms = knots$hr)
    #
    #predsbp <- -0.0211 * Gerdinsbp[, 1]
    #predsbp1 <- - 0.0379 * Gerdinsbp[, 2]
    #predsbp2 <- + 0.3386 * Gerdinsbp[, 3]
    #
    #psbp <- cbind(predsbp, predsbp1, predsbp2)
    #
    #predhr <- - 0.0030 * Gerdinhr[, 1]
    #predhr1 <- - 0.0157* Gerdinhr[, 2]
    #predhr2 <- + 0.2014* Gerdinhr[, 3]
    #
    #phr <- cbind(predhr, predhr1, predhr2)
    #
    #pred <- 2.2142 + rowSums(cbind(psbp, phr, -0.2048 * Gerdin$gcs))
    #
    #ptc <- 1/(1 + exp(-pred))
    #pb <- data.frame(pred = ptc, s30d = mdfm$s30d)
    #
    #p <- rep(NA, length(ptc))
    #p[ptc < 0.25] <- "Green"
    #p[ptc >= 0.25 & ptc < 0.50] <- "Yellow"
    #p[ptc >= 0.50 & ptc < 0.75] <- "Orange"
    #p[ptc >= 0.75] <- "Red"
    #
    #p <- factor(p, levels = c("Green", "Yellow", "Orange", "Red"))
    #
    #Gtc <- data.frame(pred = p, s30d = Gerdin[, "s30d"])
    #Gtc$id <- c(1:nrow(Gtc))
}

