#' Extract extra sample characterstics function
#'
#' This function extracts extra sample characterstistics from the raw table data, using levels of descriptive characteristics.
#' @param study_data Data frame. The study data frame. No Default.
#' @param raw_table Data.frame. Raw table data, i.e. not formatted with xtable or other latex generating function. No default.
#' @param descriptive_characterstics Character vector. Names of decriptive characteristics. Defaults to c("sex", "moi").
#' @param which_max Character vector. Which characterstic(s) to describe as max. Defaults to "moi".
#' #' @param results_list String. Name of results list, if such a list exists. Defaults to NULL.
#' @export
extract.additional.characteristics <- function(study_data, raw_table,
                                               descriptive_characteristics = c("sex", "moi"),
                                               which_max = "moi", results_list = NULL){
    ## Error handling
    if (!is.data.frame(raw_table)) stop("Raw table must be data frame.")
    ## Subset important characteristics from table
    desc_chars <- lapply(setNames(nm = descriptive_characteristics),
                              function(char){
                                  vec <- raw_table[grep(char, row.names(raw_table)),]
                              }
                              )
    ## Get levels of mechanism of injury
    levels_moi <- levels(study_data$moi)
    ## Subset moi rows from table
    moi_rows <- raw_table[unlist(lapply(levels_moi, grep, x =  raw_table[, "Level"])),]
    ## Get valus of moi from Overall column in raw.table
    subbed_ints <- unlist(lapply(levels_moi,
                                 function (the_level){
                                     the_row <- grep(the_level, raw_table[, "Level"])
                                     the_cell <- raw_table[the_row, "Overall"]
                                     subbed_cell <- as.numeric(gsub(" \\(.*", "", the_cell))
                                     return (subbed_cell)
                                 }))
    ## Get max of values and label of that moi
    the_max_moi <- lapply(setNames(c("Level", "Overall"), nm = c("Level", "Value")),
                          function(var){
                              moi_rows[which.max(subbed_ints), var]
                          })
    ## Attach to results
    if(!is.null(results_list)) {
        results_list[["the_max_moi"]] <<- the_max_moi
        results_list[["descriptive_characteristics"]] <<- desc_chars
    }
}
