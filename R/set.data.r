#' Generate complete case dataset function
#'
#' This function creates a complete case dataset.
#' @param study_data The study data frame. No default.
#' @param test Logical. If TRUE, study_data is test data. Defaults to TRUE.
#' @export
set.data <- function(
                     study_data,
                     test = TRUE
                     )
{
    ## Append empty ns list to results
    results$n_s <<- list()
    ## Coerce age to numeric
    if (!is.numeric(study_data$age)) {
        if (">89" %in% study_data$age) study_data$age[study_data$age == ">89"] <- "100"
        study_data$age <- as.numeric(study_data$age)
    }
    ## Assign number of enrolled to results
    results$n_s$n_enrolled <<- nrow(study_data)
    ## Error handling
    if (!("doar" %in% names(study_data))) stop ("Doar not in study_data")
    ## Exclude observations with informed consent no
    study_data <- study_data[study_data$ic == "Yes", ]
    results$n_s$n_ic <<- nrow(study_data)
    ## Drop ic column from data
    study_data$ic <- NULL
    if (!test){
        ## Coerce doar to date format
        study_data$doar <- as.Date(study_data$doar)
        ## drops observations included later than one month prior to creating this dataset
        study_data <- study_data[study_data$doar < "2017-05-22", ]
        results$n_s$n_before_22052017 <<- nrow(study_data)
        ## 2016-07-28 was the date when 1515 started collecting triage category
        study_data <- study_data[study_data$doar >= "2016-07-28", ]
        results$n_s$n_after_tc1515 <<- nrow(study_data)
    }
    ## Define gcs_components
    gcs_components <- c("egcs", "mgcs", "vgcs")
    ## replace non testable (99) in gcs with 1 instead 99
    study_data[, gcs_components][study_data[, gcs_components] == 99] <- 1
    ## Coerce s30d of data to numeric
    levels(study_data$s30d) <- c(0,1)
    study_data$s30d <- as.numeric(as.character(study_data$s30d))
    if (test) study_data$doar <- study_data$seqn
    study_data <- study_data[order(-study_data$s30d, study_data$doar), ]
    ## Order dataset copy according to data of arrival and s30d
    ## find the date when 200 had died
    cc <- complete.cases(study_data)
    date <- study_data[cc, ]$doar[200]
    results$n_s$n_cc <<- sum(cc)
    ## identify complete cases
    cc_df <- study_data[cc & study_data$doar <= date, ]
    results$n_s$n_when_200 <<- nrow(cc_df)
    ## and all cases
    all <- study_data[study_data$doar <= date, ]
    ## Refactor s30d for all
    all$s30d <- factor(all$s30d)
    ## Last, split data in half for training and test set (should work for Date objects)
    split_point <- floor(median(cc_df$doar)) # Define split point as the median of doar
    top_split <- cc_df[cc_df$doar < split_point, ] # Top split of data, seqn > median
    bottom_split <- cc_df[cc_df$doar < split_point, ] # Bottom split of data, seqn < median
    # Listify training and test set
    cc_dfs <- list(train = bottom_split,
                   test = top_split)
    
    return (list(cc_dfs = cc_dfs,
                 all = all))
}
