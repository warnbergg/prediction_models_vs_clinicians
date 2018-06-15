#' Bin models function
#'
#' This function bins model predictions using SupaLarnas gridsearch.breaks function.
#' @param predictions A numeric vector of predicted probabilites. No default.
#' @param outomes A numeric vector of 0 and 1. No default.
#' @param by_seq Used for "by" parameter in seq. Defaults to 1.
#' @param gridsearch_parallel Logical. If TRUE the gridsearch is performed in parallel. Defaults to FALSE.
#' @export
bin.models <- function(
                       predictions,
                       outcomes,
                       by_seq = 1,
                       gridsearch_parallel = FALSE
                       )
{
    ## Grid search cutpoints for model predictions.
    ## Use max and min of predictions in as starting and
    ## end point in grid search.
    cut_points <- SupaLarna::gridsearch.breaks(predictions,
                                               outcomes = outcomes,
                                               parallel = gridsearch_parallel,
                                               grid = seq(min(predictions),
                                                          max(predictions),
                                                          by_seq))
    ## Define labels for binning
    labels <- c("Green",
                "Yellow",
                "Orange",
                "Red")
    ## Use cut_points to bin predictions
    binned_predictions <- cut(predictions,
                              breaks = c(0,cut_points, Inf),
                              labels = labels,
                              include.lowest = TRUE)

    return (binned_predictions)

}
