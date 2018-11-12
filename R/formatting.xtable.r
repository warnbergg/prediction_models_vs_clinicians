#' Format xtable function
#'
#' This function takes a xtable print and formats.
#' @param the_xtable As xtable print. No default.
#' @param star_or_caption String. Whether to place comment as footnote or as caption. Accepted strings are "caption" and "star_caption". No Default.
#' @param table_notes String. The string to be put under the table as note. Not dynamic, only one note allowed. Defaults to NULL.
#' @export
formatting.xtable <- function(the_xtable,
                              star_or_caption = NULL,
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
    ## Initialize note variable
    note <- ""
    ## If table_notes are given, and a specific substring of the table is to be commented
    if (!is.null(table_notes) && star_or_caption == "star_caption") note <- sprintf("\\item[*] %s \n", table_notes)
    ## If table_notes are given, but no specific substring is to be commented
    if (!is.null(table_notes) && star_or_caption == "caption") note <- sprintf("\\item %s", table_notes)
    ## Insert note
    if (!is.null(table_notes)) table_notes <- paste0("\\begin{tablenotes} \\footnotesize \n",
                                                     note,
                                                     "\n",
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
