#' Make xtable function
#'
#' This function inputs table data and saves to .tex file as xtable.
#' @param table_data. Data frame. No default.
#' @param caption String. Caption for table. Defaults to "Table of estimates".
#' @param label String. Latex label. Defaults to "analysis_table".
#' @param file_name String. File name. No default.
#' @export
make.xtable <- function(table_data,
                        caption = "Table of Estimates.", label = "analysis_table",
                        include_rownames = TRUE, file_name){
    ## xtable the analysis table
    the_table <- xtable::print.xtable(xtable::xtable(table_data,
                                                     caption = paste("\bf", caption),
                                                     label = paste0("tab:", label)),
                                      type = "latex",
                                      booktabs = TRUE,
                                      table.placement = "!ht",
                                      include.rownames = include_rownames,
                                      include.colnames = TRUE,
                                      caption.placement = "top",
                                      print.results = FALSE)
    ## Format, i.e. add tabular and adjustbox environments
    the_table <- formatting.xtable(the_table)
    ## Save table
    write(the_table, file = file_name)
    }
