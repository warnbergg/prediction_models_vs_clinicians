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
    results$n_ic <<- nrow(study_data)
    ## Drop ic column from data
    study_data$ic <- NULL
    ## Coerce doar to date format
    study_data$doar <- as.Date(study_data$doar)
    ## drops observations included later than one month prior to creating this dataset
    study_data <- study_data[study_data$doar < "2017-05-22", ]
    results$n_before_22052017 <<- nrow(study_data)
    ## 2016-07-28 was the date when 1515 started collecting triage category
    study_data <- study_data[study_data$doar >= "2016-07-28", ]
    results$n_after_tc1515 <<- nrow(study_data)
    ## Define gcs_components
    gcs_components <- c("egcs", "mgcs", "vgcs")
    ## replace non testable (99) in gcs with 1 instead 99
    study_data[, gcs_components][study_data[, gcs_components] == 99] <- 1
    ## Coerce s30d of data to numeric
    levels(study_data$s30d) <- c(0,1)
    study_data$s30d <- as.numeric(as.character(study_data$s30d))
    ## Order dataset copy according to data of arrival and s30d
    study_data <- study_data[order(-study_data$s30d, study_data$doar), ]
    ## find the date when 200 had died
    cc <- complete.cases(study_data)
    date <- study_data[cc, ]$doar[200]
    results$n_cc <<- sum(cc)
    ## identify complete cases
    cc_df <- study_data[cc & study_data$doar <= date, ]
    results$n_when_200 <<- nrow(cc_df)
    ## and all cases
    all <- study_data[study_data$doar <= date, ]
    ## Refactor s30d for all
    all$s30d <- factor(all$s30d)

    return (list(cc_df = cc_df,
                 all = all))
}
