#' Extract extra sample characterstics function
#'
#' This function extracts descriptive sample characterstistics from the raw table data using the study data.
#' @param study_data Data frame. The study data frame. No Default.
#' @param raw_table Data.frame. Raw table data, i.e. not formatted with xtable or other latex generating function. No default.
#' @param descriptive_characterstics Character vector. Names of decriptive characteristics. Defaults to c("sex", "moi").
#' @param results_list String. Results list, i.e. list containing results from analysis. Defaults to NULL.
#' @export
extract.additional.characteristics <- function(study_data, raw_table,
                                               descriptive_characterstics = c("sex", "moi"),
                                               results_list = NULL){
    ## Error handling
    if (is.null(results_list)) stop("Name of results list must be specified.")
    ## Subset important characteristics from table
    desc_chars <- lapply(setNames(nm = descriptive_characterstics),
                         function(char){
                             ## Extract levels of characteristic from
                             ## study data
                             char_levels <- levels(study_data[, char])
                             ## Subset rows using levels
                             char_rows <- raw_table[unlist(lapply(char_levels,
                                                                  grep,
                                                                  x = raw_table[, "Level"])),]
                             ## Get value of levels of characteristic from
                             ## overall column in raw table
                             vals_wo_parantheses <- unlist(lapply(
                                 char_levels, function(the_level){
                                     ## Subset row
                                     the_row <- grep(the_level, raw_table[, "Level"])
                                     ## Extract cell corresponding with Overall col
                                     the_cell <- raw_table[the_row, "Overall"]
                                     ## Remove parantheses to extract value
                                     subbed_cell <- as.numeric(gsub(" \\(.*", "", the_cell))
                                     return (subbed_cell)
                                 }))
                             ## Extract max value and label of char
                             the_max_char <- lapply(
                                 setNames(c("Level", "Overall"), nm = c("Level", "Value")), function(var){
                                     cell <- char_rows[which.max(vals_wo_parantheses), var]
                                     if (var == "Level") cell <- tolower(cell)
                                     return (cell)
                             })
                             return (the_max_char)
                         })
    ## Attach to results
    results$desc_characteristics <<- desc_chars
}
