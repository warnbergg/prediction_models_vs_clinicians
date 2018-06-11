## Get packages function
                                        #
# This function gets packages
#' @export
load.packages <- function(){
    ## List packages
    packages <- c("ggplot2",
                  "ROCR",
                  "knitr",
                  "xtable")
    ##Load packages
    for (package in packages) require(package,
                                      character.only = TRUE)
}
