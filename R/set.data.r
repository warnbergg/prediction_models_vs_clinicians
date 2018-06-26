#' Generate complete case dataset function
#'
#' This function creates a complete case dataset.
#' @param study_data The study data frame. No default.
#' @export
set.data <- function(
                     study_data
                     )
{
    ## Assign number of enrolled to results
    results$n_enrolled <<- nrow(study_data)
    ## Error handling
    if (!("doar" %in% names(study_data))) stop ("Doar not in study_data")
    ## Exclude observations with informed consent no
    study_data <- study_data[study_data$ic == "Yes", ]
    ## Drop ic column from data
    study_data$ic <- NULL
    ## Coerce doar to date format
    study_data$doar <- as.Date(study_data$doar)
    ## drops observations included later than one month prior to creating this dataset
    study_data <- study_data[study_data$doar < "2017-05-22", ]
    ## 2016-07-28 was the date when 1515 started collecting triage category
    study_data <- study_data[study_data$doar >= "2016-07-28", ]
    ## Define gcs_components
    gcs_components <- c("egcs", "mgcs", "vgcs")
    ## replace non testable (99) in gcs with 1 instead 99
    study_data[, gcs_components][study_data[, gcs_components] == 99] <- 1
    ## find the date when 200 had died
    cc <- complete.cases(study_data)
    date <- study_data[cc, ]$doar[200]
    ## identify complete cases
    cc_df <- study_data[cc & study_data$doar <= date, ]
    ## and all cases
    all <- study_data[study_data$doar <= date, ]
    ## Add number of patients after cc to results
    results$n_after_exclusion <<- nrow(cc_df)

    return (list(cc_df = cc_df,
                 all = all))
}
