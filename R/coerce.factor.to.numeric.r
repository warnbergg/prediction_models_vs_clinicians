#' Coerce factors to numeric function
#'
#' This function coerces specified variables to numeric from factor.
#' @param study_data Study data list. No default.
#' @param vars Character vector. Names of variables to coerce to numeric. Defaults to c("egcs", "mgcs", "vgcs", "avpu").
#' @export
coerce.factor.to.numeric <- function(study_data, vars = c("egcs", "mgcs", "vgcs", "avpu")){
    ## Error handling
    if (all(vars %in% study_data)) stop("All vars not in study_data.")
    ## Transform some factors to numeric
    study_data[, vars] <- lapply(study_data[, vars],
                                 function(comp) as.numeric(comp))

    return (study_data)
}
