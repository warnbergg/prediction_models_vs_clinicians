## Get packages function
                                        #
# This function gets packages
#' @export
getPackages <- function(){

    packages <- c("ggplot2",
                  "ROCR",
                  "knitr",
                  "xtable")
    for (package in packages) require(package,
                                      character.only = TRUE)
}
