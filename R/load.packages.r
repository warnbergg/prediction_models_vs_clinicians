## Get packages function
                                        #
# This function gets packages
#' @export
load.packages <- function(){
    ## List packages
    packages <- c("SupaLarna")
    ##Load packages
    for (package in packages) require(package,
                                      character.only = TRUE)
}