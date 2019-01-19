#' Generate table one function
#'
#' This function uses SupaLarnas create.table.of.sample.charactersitics to create table one, and binds with a column of missing values.
#' @param study_data Data frame. The study data. No default
#' @param data_dictionary. The data_dictionary. No default.
#' @param digits Integer. Determining number of decimals when rounded. Defaults to 2.
#' @param save Logical. Whether to write the table as latex. Defaults to TRUE.
#' @export
generate.tbl.one <- function(study_data, data_dictionary,
                             digits = 1, save = TRUE){
    ## Setup tbl one with
    ## SupaLarnas create.table.of.sample.characteristics
    tables <- SupaLarna::create.table.of.sample.characteristics(study_data,
                                                                data_dictionary,
                                                                strata = "set",
                                                                include_missing_column = FALSE)
    ## Get indices of strata colnames in raw table colnames
    level_indices <- unlist(lapply(levels(study_data$s30d),
                                   function(exp) grep(exp, colnames(tables$raw))))
    ## Assign those to the colnames of the raw table
    colnames(tables$raw)[level_indices] <- c("Gridsearch", "Comparison")
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
