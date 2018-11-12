#' Make xtable function
#'
#' This function inputs table data and saves to .tex file as xtable.
#' @param table_data. Data frame. No default.
#' @param file_name String. File name. No default.
#' @param caption String. Caption for table. Defaults to "Table of estimates".
#' @param label String. Latex label. Defaults to "analysis_table".
#' @param star_substring Character vector. The substrings of xtable to be commented under the table. Defaults to NULL.
#' @param table_notes String. Note to be put under table. Not dynamic, only one note allowed. If star_substring is NULL, a caption is placed under the table. Otherwise, the comment is placed as a footnote. Passed to formatting.xtable. Defaults to NULL.
#' @export
make.and.save.xtable <- function(table_data, file_name,
                                 caption = "Table of Estimates.", label = "analysis_table",
                                 star_substring= NULL,
                                 table_notes = NULL,
                                 ...){
    ## xtable the analysis table
    the_table <- xtable::print.xtable(xtable::xtable(table_data,
                                                     caption = caption,
                                                     label = paste0("table:", label),
                                                     ...),
                                      type = "latex",
                                      booktabs = TRUE,
                                      table.placement = "!ht",
                                      include.colnames = TRUE,
                                      caption.placement = "top",
                                      print.results = FALSE,
                                      ...)
    ## Subset table contents from xtable string
    split_table <- strsplit(the_table, "\\toprule\n", fixed = TRUE)
    ## Extract body of table
    table_body <- split_table[[1]][2]
    star_or_caption <- ""
    if (!is.null(table_notes)){
        star_or_caption <- "caption" # Table footnote without asterisk
        if (!is.null(star_substring)){
            star_or_caption <- "star_caption" # Table footnote with asterisk
            for (substring in star_substring){
                table_body <- sub(substring,    
                                  paste0(substring,
                                         "*"),
                                  table_body)
            }
            ## Merge the split
            the_table <- paste0(split_table[[1]][1], table_body)
        }
    }
    ## Format, i.e. add tabular, adjustbox, and threeparttable environments
    the_table <- formatting.xtable(the_table, table_notes = table_notes,
                                   star_or_caption = star_or_caption)
    ## Save table
    write(the_table, file = file_name)
}
