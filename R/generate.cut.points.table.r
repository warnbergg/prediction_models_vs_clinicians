#' Generate cut points table function
#'
#' This function generates the cut points table.
#' @param cut_points List of numeric vectors. Each containing cut points for models. No default.
#' @param invert_names Character vector. Labels of predictions to be inverted. Defaults to c("RTS", "GAP", "KTS").
#' @param save Logical. Save to disk? Defaults to TRUE.
#' @export
generate.cut.points.table <- function(cut_points,
                                      invert_names = c("RTS", "GAP", "KTS"), save = TRUE){
    ## Error handling
    if (!is.list(cut_points)) stop ("Cut_points must be list.")
    if (!all(lapply(invert_names, function(pred_name) pred_name%in% names(cut_points)))) stop("All invert_names not in cut_points list")
    ## Invert prediction listed in invert_names
    for (pred_label in invert_names){
        cut_points[[pred_label]] <- rev(1/cut_points[[pred_label]])
    }
    if (save) make.xtable(do.call(cbind, cut_points),
                          file_name = "cut_points_table.tex",
                          caption = "Cut points optimising auc.",
                          label = "cut_points",
                          include_rownames = FALSE)

    return (do.call(cbind, cut_points))
}
