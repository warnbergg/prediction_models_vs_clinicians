#' Bin models function
#'
#' This function bins model predictions using SupaLarnas gridsearch.breaks function.
#' @param predictions A numeric vector of predicted probabilites. No default.
#' @param outomes A numeric vector of 0 and 1. No default.
#' @param grid All values to be combnd in gridsearch.breaks, i.e. used to find optimal cutpoints. No default.
#' @param n_cores Number of cores to be used in parallel gridsearch. Passed to SupaLarna::gridsearch.breaks. As integer. Defaults to 2 (in gridsearch.breaks)
#' @param gridsearch_parallel Logical. If TRUE the gridsearch is performed in parallel. Defaults to FALSE.
#' @export
bin.models <- function(
                       predictions,
                       outcomes,
                       n_cores,
                       gridsearch_parallel = FALSE
                       )
{
    ## Grid search cutpoints for model predictions.
    ## Use max and min of predictions in as starting and
    ## end point in grid search.
    cut_points <- SupaLarna::gridsearch.breaks(predictions,
                                               grid = seq(min(predictions),
                                                          max(predictions),
                                                          0.01),
                                               outcomes = outcomes,
                                               parallel = gridsearch_parallel,
                                               n_cores = n_cores,
                                               sample = TRUE)
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
