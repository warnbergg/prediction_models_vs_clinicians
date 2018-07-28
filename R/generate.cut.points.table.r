#' Generate cut points table function
#'
#' This function generates the cut points table.
#' @param cut_points List of numeric vectors. Each containing cut points for models. No default.
#' @param range_labels Character vector. Labels to ranges cut by cut points. Defaults to c("Green", "Yellow", "Orange", "Red").
#' @param invert_names Character vector. Labels of predictions to be inverted. Defaults to c("RTS", "GAP", "KTS").
#' @param digits Integer. Number of decimals when cell values are rounded. Defaults to 2.
#' @param save Logical. Save to disk? Defaults to TRUE.
#' @export
generate.cut.points.table <- function(cut_points,
                                      range_labels = c("Green", "Yellow", "Orange", "Red"),
                                      invert_names = c("RTS", "GAP", "KTS"), digits = 2,
                                      save = TRUE){
    cut_points = results$cut_points_lst
    ## Error handling
    if (!is.list(cut_points)) stop ("Cut_points must be list.")
    ## Invert prediction listed in invert_names
    for (pred_label in invert_names){
        cut_points[[pred_label]] <- rev(cut_points[[pred_label]])
    }
    ## Generate table
    tbl <- as.matrix(do.call(cbind, cut_points))
    ## Initialize list for rows
    row_lst <- list()
    ## Adding operators. Loop through 1 to nrow of table + 1, adding
    ## operators besed on type of score
    for (i in 0:nrow(tbl) + 1){
        ## Get logical. To minimise repeating.
        to_invert <- invert_names[i] %in% colnames(tbl)[i]
        if (i == 1){
            ## Flip operator for max to min scores
            ## and tos
            operator <- "<"
            if (to_invert) operator <- ">"
            ## First iter
            row_lst[[i]] <- paste(operator, round(tbl[i,],
                                                  digits = digits))
        } else if (i > 1 && i < nrow(tbl) + 1){
            ## Subsequent iters until nrow(tbl) - 1
            row_lst[[i]] <- paste(round(tbl[i - 1,], digits = digits),
                                  "to",
                                  round(tbl[i,],
                                        digits = digits))
        } else if (i == nrow(tbl) + 1) {
            ## Last iter
            row_lst[[i]] <- paste(operator, round(tbl[i - 1,],
                                                  digits = digits))
        }
    }
    ## Bind levels
    range_tbl <- data.frame(do.call(rbind, row_lst),
                            row.names = range_labels)
    colnames(range_tbl) <- colnames(tbl)
    ## Save to disk
    if (save) make.and.save.xtable(range_tbl,
                                   file_name = "cut_points_table.tex",
                                   caption = "Included models and corresponding cut points optimising auc.",
                                   label = "cut_points")

    return (range_tbl)
}
