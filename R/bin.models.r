#' Bin models function
#'
#' This function bins model predictions using SupaLarnas gridsearch.breaks function.
#' @param predictions A numeric vector of predicted probabilites. No default.
#' @param outcomes A numeric vector of 0 and 1. No default.
#' @param n_cores Number of cores to be used in parallel gridsearch. Passed to SupaLarna::gridsearch.breaks. As integer. Defaults to 2 (in gridsearch.breaks)
#' @param grid All values to be combnd in gridsearch.breaks, i.e. used to find optimal cutpoints. No default.
#' @param return_cps Logical. Function returns model cut_points if TRUE. Defaults to TRUE.
#' @param gridsearch_parallel Logical. If TRUE the gridsearch is performed in parallel. Defaults to FALSE.
#' @param is_sample Logical. If TRUE, only a tenth of possible cut points is searched. Defaults to TRUE.
#' @param maximise Logical. If TRUE, grid search maximises performance metric. Passed to SupaLarna::gridsearch.breaks. Defaults to TRUE.
#' @export
bin.models <- function(
                       predictions,
                       outcomes,
                       n_cores,
                       grid,
                       return_cps = TRUE,
                       gridsearch_parallel = FALSE,
                       is_sample = TRUE,
                       maximise = FALSE
                       )
{
    ## Define labels for later binning of predictions
    labels <- c("Green",
                "Yellow",
                "Orange",
                "Red")
    ## Grid search cutpoints for model predictions.
    ## Use max and min of predictions in as starting and
    ## end point in grid search.
    cut_points <- SupaLarna::gridsearch.breaks(
                                 predictions$train,
                                 grid = grid,
                                 outcomes = outcomes,
                                 parallel = gridsearch_parallel,
                                 n_cores = n_cores,
                                 sample = is_sample,
                                 maximise = maximise)
    if (return_cps) results$cut_points_lst[[paste0(max(predictions$train), "_cps")]] <<- cut_points
    ## Use cut_points to bin train and test predictions
    binned_predictions <- lapply(predictions, function(preds)
        cut(preds,
            breaks = c(0,cut_points, Inf),
            labels = labels,
            include.lowest = TRUE))
    ## Return the binned predictions
    return (binned_predictions)
}
