## Get packages function
#'
#' This function imports packages.
#' @export
load.packages <- function(){
    ## List packages
    packages <- c("SupaLarna",
                  "knitr",
                  "splitstackshape")
    ##Load packages
    for (package in packages) require(package,
                                      character.only = TRUE)
}
