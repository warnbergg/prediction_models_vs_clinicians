#' Split data function
#'
#' This function sets the data for analysis, i.e. splits into to two datasets for training and test.
#' @param cc_df Data frame. The complete case data. No default.
#' @export
split.data <- function(
                     cc_df
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
    
    return (return_object)
}
