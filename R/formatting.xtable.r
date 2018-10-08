#' Format xtable function
#'
#' This function takes a xtable print and formats.
#' @param the_xtable As xtable print. No default.
#' @param table_notes String. The string to be put under the table as note. Not dynamic, only one note allowed. Defaults to NULL.
#' @export
formatting.xtable <- function(the_xtable,
                              table_notes = NULL){
    ## Error handling
    if (!is.null(table_notes) && !(length(table_notes) == 1)) stop("Table notes should be of length 1.")
    ## Add begin tabular and adjustbox environments
    the_xtable <- sub("\\begin{tabular}",
                      paste0("\\begin{adjustbox}{max width = \\linewidth} \n",
                             "\\begin{threeparttable} \n",
                             "\\begin{tabular} \n"),
                           the_xtable,
                      fixed = TRUE)
    ## Define table notes in tablenotes call
    if (!is.null(table_notes)) table_notes <- paste0("\\begin{tablenotes} \\footnotesize \n",
                                                     sprintf("\\item[*] %s \n", table_notes),
                                                     "\\end{tablenotes} \n")
    ## Add end to tabular and adjustbox environments
    the_xtable <- sub("\\end{tabular}",
                      paste0("\\addlinespace \n",
                             "\\end{tabular} \n",
                             table_notes,
                             "\\end{threeparttable} \n",
                             "\\end{adjustbox}"),
                           the_xtable,
                      fixed = TRUE)
    return (the_xtable)
    }
