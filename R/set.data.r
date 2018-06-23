#' Generate complete case dataset function
#'
#' This function creates a complete case dataset.
#' @param study_data The study data frame. No default.
#' @export
set.data <- function(
                     study_data
                     )
{
    ## Error handling
    if (!("doar" %in% names(study_data))) stop ("Doar not in study_data")
    ## Coerce doar to date format
    study_data$doar <- as.Date(study_data$doar)
    ## drops observations included later than one month prior to creating this dataset
    study_data <- study_data[study_data$doar < "2017-05-22", ]
    ## 2016-07-28 was the date when 1515 started collecting triage category
    study_data <- study_data[study_data$doar >= "2016-07-28", ]
    ## replace non testable (99) in gcs with 1 instead 99
    study_data[, c("egcs", "mgcs", "vgcs")][study_data[, c("egcs", "mgcs", "vgcs")] == 99] <- 1
    ## find the date when 200 had died
    cc <- complete.cases(study_data)
    date <- study_data[cc, ]$doar[200]
    ## identify complete cases
    cc_df <- study_data[cc & study_data$doar <= date, ]
    ## Count number of patients omitted
    omitted <- nrow(study_data[study_data$doar <= date, ]) - nrow(cc_df)
    ## Add number of omitted and subjects in dataset to results
    ## in mother function
    results$n_dataset <<- nrow(study_data)
    results$n_after_omit <<- nrow(cc_df)

    return (cc_df)
}
