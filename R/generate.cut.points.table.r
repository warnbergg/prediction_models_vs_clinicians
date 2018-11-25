#' Generate cut points table function
#'
#' This function generates the cut points table.
#' @param cut_points List of numeric vectors. Each containing cut points for models. No default.
#' @param group_labels Character vector. Labels to groups. Defaults to c("Green", "Yellow", "Orange", "Red").
#' @param invert_names Character vector. Labels of predictions to be inverted. Defaults to c("RTS", "GAP", "KTS").
#' @param digits Integer. Number of decimals when cell values are rounded. Defaults to 2.
#' @param save Logical. If TRUE, table is saved to disk. Defaults to TRUE.
#' @param table_notes String. Notes to be put under table. Passed to make.and.save.xtable. Defults to "RTS Revised Trauma Score; GAP Glasgow Coma Scale, Age, Systolic Blood Pressure; KTS Kampala Trauma Score"
#' @export
generate.cut.points.table <- function(cut_points,
                                      group_labels = c("Green", "Yellow", "Orange", "Red"),
                                      invert_names = c("RTS", "GAP", "KTS"), digits = 2,
                                      save = TRUE,
                                      table_notes= "Abbreviations: RTS Revised Trauma Score; GAP Glasgow Coma Scale, Age, Systolic Blood Pressure; KTS Kampala Trauma Score"){
    ## Error handling
    if (!is.list(cut_points)) stop ("Cut_points must be list.")
    ## Invert prediction listed in invert_names
    for (pred_label in invert_names){
        cut_points[[pred_label]] <- rev(cut_points[[pred_label]])
    }
    ## Generate cut points column for cut points table
    cutpoints_mtrx <- do.call(cbind, lapply(seq_along(cut_points), function (iter){
        ## Define length of cut points list element
        ## plus one, to make a list of length plus one
        len_plus_one <- length(cut_points[[iter]]) + 1
        ## Define logical if to invert. To minimise repeating lines.
        to_invert <- names(cut_points)[iter] %in% invert_names[iter]
        sapply(1:len_plus_one, function (jiter){
            ## For first cut point
            if (jiter == 1){
                ## Define operator based on invert cut points or not
                operator <- "<"
                if (to_invert) operator <- ">"
                return_object <- paste(operator,
                                       round(cut_points[[iter]][jiter], digits = digits))
            ## For subsequent cut points and
            } else if (jiter > 1 && jiter < len_plus_one){
                return_object <- paste(round(cut_points[[iter]][jiter], digits=digits),
                                       "to",
                                       round(cut_points[[iter]][jiter - 1], digits = digits))
            ## For last cut point
            } else if (jiter == len_plus_one){
                ## Define operator based on invert cut points or not
                operator <- ">"
                if (to_invert) operator <- "<"
                return_object <- paste(operator,
                                       round(cut_points[[iter]][jiter - 1], digits = digits))
            }
            return (return_object)
        })
    }))
    ## Set rownames and colnames
    cutpoints_df <- data.frame(cutpoints_mtrx,
                               row.names = group_labels)
    colnames(cutpoints_df) <- names(cut_points)
    ## Set row names as first column
    ## Save to disk
    if (save) make.and.save.xtable(table_data = cutpoints_df,
                                   file_name = "cut_points_table.tex",
                                   caption = "Cut points identified with grid search.",
                                   label = "cut_points",
                                   table_notes = table_notes,
                                   align = rep("l", length(group_labels) + 1))
    return (cutpoints_df)
}
