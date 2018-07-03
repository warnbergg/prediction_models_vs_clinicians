#' Create results table function
#'
#' This function creates the results table, i.e. AUC and NRI for each model
#' @param analysis_lst List. Estimates of models. No default.
#' @export
create.results.table <- function(analysis_lst){
    ## Merge reclassifciation columns to one ci column
    analysis_lst <- lapply(analysis_lst, function(analysis_type){
        lapply(analysis_type, function (model_df) {
            ## Merge columns to paranthesised ci
            model_df$ci <- with(model_df, paste(sample_estimate,
                                                paste0("(",
                                                       paste(lower, "to", upper),
                                                       ")")))
            ## Remove original columns
            model_df[, c("lower", "upper", "sample_estimate")]<- NULL

            return(model_df)
        })
    })
    ## Tabulate reclassification
    reclassification_table <- as.data.frame(analysis_lst$reclassification)
    colnames(reclassification_table) <- names(analysis_lst$reclassification)
    ## Tabulate auc
    auc_table <- t(do.call(rbind, analysis_lst$AUROCC))
    ## Fill reclassification table for analysis table, and
    ## auc table according to vice versa
    filled_reclass <- cbind(reclassification_table,
                            matrix(rep(rep("", nrow(reclassification_table)),
                                       ncol(reclassification_table)),
                                   ncol = ncol(reclassification_table)))
    ## Set full colnames
    colnames(filled_reclass) <- colnames(auc_table)
    ## Merge reclass and auc tables
    analysis_table <- rbind(AUROCC = auc_table, filled_reclass)
    ## xtable the analysis table
    the_table <- xtable::print.xtable(xtable::xtable(analysis_table,
                                                     caption = "\\bf Table of estimates.",
                                                     label = "tab:analysis_table"),
                                      type = "latex",
                                      booktabs = TRUE,
                                      table.placement = "!ht",
                                      include.rownames = TRUE,
                                      include.colnames = TRUE,
                                      caption.placement = "top",
                                      print.results = FALSE)
    ## Format, i.e. add tabular and adjustbox environments
    the_table <- formatting.xtable(the_table)
    ## Save table
    write(the_table, "analysis_table.tex")

    return (analysis_table)
}
