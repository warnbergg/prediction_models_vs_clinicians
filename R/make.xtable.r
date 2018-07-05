#' Title of function
#'
#' Description of function.
#' @param x Integer. Short description. Default/No default.
#' @param y String. Short description. Default/No default.
#' @param z Logical. Short description. Defaults to TRUE.
#' @export
make.xtable <- function(table_data, caption, label, ){
    ## xtable the analysis table
    the_table <- xtable::print.xtable(xtable::xtable(table_data
                                                     caption = "\\bf Table of estimates.",
                                                     label = "tab:analysis_table"),
                                      type = "latex",
                                      booktabs = TRUE,
                                      table.placement = "!ht",
                                      include.rownames = TRUE,
                                      include.colnames = TRUE,
                                      caption.placement = "top",
                                      print.results = FALSE)
    ## Format, i.e. add tabular and adjustbox environments
    the_table <- formatting.xtable(the_table)
    ## Save table
    write(the_table, "analysis_table.tex")
    }
