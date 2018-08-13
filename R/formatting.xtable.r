#' Format xtable function
#'
#' This function takes a xtable print and formats.
#' @param the_xtable As xtable print. No default.
#' @export
formatting.xtable <- function(the_xtable,
                              minipage = TRUE){
    ## Add begin tabular and adjustbox environments
    the_xtable <- sub("\\begin{tabular}",
                           paste0("\\begin{adjustbox}{max width = \\linewidth} \n",
                                  "\\begin{tabular} \n"),
                           the_xtable,
                      fixed = TRUE)
    ## Add end to tabular and adjustbox environments
    the_xtable <- sub("\\end{tabular}",
                           paste0("\\end{tabular} \n",
                                  "\\end{adjustbox}"),
                           the_xtable,
                      fixed = TRUE)
    return (the_xtable)
    }
