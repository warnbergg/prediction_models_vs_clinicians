#' Generate complete case dataset function
#'
#' This function creates a complete case dataset.
#' @param study_data The study data frame. No default.
#' @export
complete.dataset <- function(
                             study_data
                             )
{
    ## Generate complete case dataset
    cc <- na.omit(study_data)
    ## Count number of patients omitted
    omitted <- nrow(study_data) - nrow(cc)
    ## Add number of omitted and subjects in dataset to results
    ## in mother function
    results$n_dataset <<- nrow(study_data)
    results$n_after_omit <<- nrow(cc)

    return (cc)
}
