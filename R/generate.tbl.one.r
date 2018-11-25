#' Generate table one function
#'
#' This function uses SupaLarnas create.table.of.sample.charactersitics to create table one, and binds with a column of missing values.
#' @param all. The all data frame. No default.
#' @param data_dictionary. The data_dictionary. No default.
#' @param digits Integer. Determining number of decimals when rounded. Defaults to 2.
#' @param save Logical. Whether to write the table as latex. Defaults to TRUE.
#' @export
generate.tbl.one <- function(all, data_dictionary,
                             digits = 2, save = TRUE){
    ## Setup tbl one with
    ## SupaLarnas create.table.of.sample.characteristics
    tables <- SupaLarna::create.table.of.sample.characteristics(all,
                                                                data_dictionary,
                                                                strata = "s30d",
                                                                include_missing_column = FALSE)
    ## Change colnames of strata columns
    ## Get indices of strata colnames in raw table colnames
    level_indices <- unlist(lapply(levels(all$s30d),
                                   function(exp) grep(exp, colnames(tables$raw))))
    ## Assign those to the colnames of the raw table
    colnames(tables$raw)[level_indices] <- c("Survivors", "Non-Survivors")
    ## Subset variables from data_dictionary to be included in table one
    incl_vars <- data_dictionary[sapply(data_dictionary,
                                        function(var) var$incl == "Yes")]
    ## Generate missing column
    missing_col <- unlist(lapply(all[, names(incl_vars)], ## Loop subset of all df
                                 function (column){
                                     ## Get number of NAs
                                     n_NAs <- sum(is.na(column))
                                     ## Get perc NAs
                                     perc_NAs <- sum(is.na(column))/length(column) * 100
                                     ## Round percentage
                                     perc_NAs <- round(perc_NAs,
                                                       digits = digits)
                                     ## Format missing
                                     NAs_fmt <- paste(n_NAs,
                                                      paste0("(", perc_NAs, ")"))
                                     return (NAs_fmt)
                                 }))
    ## Generate total missing values cell
    n <- sum(as.numeric(gsub(" \\(.*", "", missing_col)))
    n_perc <- paste0("(", round(n/nrow(all), digits = digits), ")")
    n_fmt <- paste(n, n_perc)
    ## Add total missing values to missing_col
    missing_col <- c(n = n_fmt,
                     missing_col)
    ## Remove explanatry parantheses from table rownames
    removed_ends <- sapply(rownames(tables$raw),
                           gsub,
                           pattern = " \\(.*",
                           replacement = "")
    ## Create vector the same length as raw table, and insert missing at correct
    ## index)
    counter <- 1 ## Intialize counter
    vec <- ""    ## Initialize vec
    for (cut_row_name in removed_ends){
        if (cut_row_name %in% names(missing_col)){ ## Check if name in missing col
            ## Assign missing percent to corresponding to var indice
            indice <- grep(cut_row_name, removed_ends)
            name_indice <- grep(cut_row_name, names(missing_col))
            vec[indice] <- missing_col[name_indice]
            counter <- counter + 1
        } else{
            vec[counter] <- ""
            counter <- counter + 1
        }
    }
    ## Coerce to matrix
    vec_as_matrix <- matrix(vec)
    rownames(vec_as_matrix) <- NULL                           ## Remove rownames for cbind
    colnames(vec_as_matrix) <- "Number of missing values (%)" ## Change name from vec
    ## Bind column of missing values to raw table
    tables$raw <- cbind(tables$raw, vec_as_matrix)
    ## Format table using xtable
    formatted_table <- xtable::print.xtable(xtable::xtable(tables$raw,
                                                           caption = "Sample characteristics.",
                                                           label = "tab:sample-characteristics"),
                                            type = "latex",
                                            booktabs = TRUE,
                                            table.placement = "!ht",
                                            include.rownames = FALSE,
                                            include.colnames = TRUE,
                                            caption.placement = "top",
                                            print.results = FALSE)
    ## Add adjustbox and tabular
    formatted_table <- formatting.xtable(formatted_table,
                                         star_or_caption = "caption",
                                         table_notes = tables$caption)
    ## New tables list
    tables <- list(raw = tables$raw,
                   formatted_table)
    if (save) write(formatted_table, "table_of_sample_characteristics.tex")

    return (tables)
}
