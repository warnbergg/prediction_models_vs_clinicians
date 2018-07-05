#' Create results table function
#'
#' This function creates the results table, i.e. tabulates condfidence intervals and sample estimates for models.
#' @param analysis_objs List of data frames. Each containing ci around estimates by models. No default.
#' @export
create.results.table <- function(analysis_objs){
    ## Error handling
    if (!(all(unlist(lapply(analysis_objs, is.data.frame))))) stop ("Argument must be list of data frames")
    ## Merge analysis_objs list to single data.frame
    merged <- do.call(cbind, analysis_objs)

    return (merged)
}
