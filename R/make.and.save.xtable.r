#' Make xtable function
#'
#' This function inputs table data and saves to .tex file as xtable.
#' @param table_data. Data frame. No default.
#' @param san_row Function. Identity function for sanitize.rownames.function. Defaults to NULL.
#' @param san_col Function. Identity function for sanitize.colnames.function. Defaults to NULL.
#' @param caption String. Caption for table. Defaults to "Table of estimates".
#' @param label String. Latex label. Defaults to "analysis_table".
#' @param file_name String. File name. No default.
#' @param table_notes String. Note to be put under table. Not dynamic, only one note allowed. Passed to formatting.xtable. Defaults to NULL.
#' @param star_caption String. The substring of xtable to be commented under the table. Defaults to NULL.
#' @export
make.and.save.xtable <- function(table_data, file_name, san_row = NULL, san_col = NULL,
                                 caption = "Table of Estimates.", label = "analysis_table",
                                 include_rownames = TRUE, table_notes = NULL,
                                 star_caption = NULL){
    ## xtable the analysis table
    the_table <- xtable::print.xtable(xtable::xtable(table_data,
                                                     caption = paste("\\bf", caption),
                                                     label = paste0("table:", label)),
                                      type = "latex",
                                      booktabs = TRUE,
                                      table.placement = "!ht",
                                      include.rownames = include_rownames,
                                      include.colnames = TRUE,
                                      sanitize.rownames.function = san_row,
                                      sanitize.colnames.function = san_col,
                                      caption.placement = "top",
                                      print.results = FALSE)
    ## Add star to the specified substring of the_table
    if (!(is.null(star_caption) && !(is.null(table_notes)))) the_table <- gsub(star_caption,
                                                                              paste0(star_caption, "*"),
                                                                              the_table)
    ## Format, i.e. add tabular and adjustbox environments
    the_table <- formatting.xtable(the_table, table_notes = table_notes)
    ## Save table
    write(the_table, file = file_name)
    }
