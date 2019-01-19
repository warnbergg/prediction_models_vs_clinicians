#' Split data function
#'
#' This function sets the data for analysis, i.e. splits into to two datasets for training and test.
#' @param cc_df Data frame. The complete case data. No default.
#' @param for_tbl_one Logic. If TRUE, merged table is return with strata variable "set". Defaults to TRUE.
#' @export
split.data <- function(
                       cc_df,
                       for_tbl_one = TRUE
                       )
{
    ## Error handling
    if (!is.data.frame(cc_df)) stop("The study_data is not of data frame type.")
    cc_df <- cc_df[order(-cc_df$s30d, cc_df$seqn), ] # Order by mortality and sequential int
    seqn_when_200 <- cc_df$seqn[200] # Seqn when 200 patients had died
    seqn_when_400 <- cc_df$seqn[400] # Seqn when 400 patients had died
    train <- cc_df[cc_df$seqn <= seqn_when_200, ] # For training
    test <- cc_df[cc_df$seqn > seqn_when_200 & cc_df$seqn <= seqn_when_400, ] # For testing
    # Listify training and test set
    return_object <- list(train = train,
                          test = test)
    if (for_tbl_one) {
        train_w_strata <- cbind(train, set = rep("Gridsearch", nrow(train)))  # Add strata
        test_w_strata <- cbind(test, set = rep("Comparison", nrow(test)))     # Add strata
        merged <- rbind(train_w_strata, test_w_strata)                        # Merge tables 
        merged$s30d <- factor(merged$s30d)                                    # Coerce s30d to factor 
        levels(merged$s30d) <- c("No", "Yes")                                 # And change levels
        return_object$merged <- merged                                        # Add to return
    }
    
    return (return_object)
}
