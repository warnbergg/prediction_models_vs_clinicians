#' Generate model variables table
#'
#' This function generates the table that includes the models, and the variables to be input.
#' @param col_names Integer. Short description. Default/No default.
#' @export 
generate.variables.table <- function(col_names){
    ## Define variables included in each model
    RTS <- c("Systolic blood pressure",
             "Glasgow Coma Scale",
             "Respiratory rate",
             "")
    GAP <- c("Systolic blood pressure",
             "Glasgow Coma Scale",
             "Age",
             "")
    KTS <- c("Systolic blood pressure",
             "Respiratory rate",
             "Age",
             "Number of serious injuries")
    Gerdin <- c("Systolic blood pressure",
                "Heart rate",
                "Glasgow Coma Scale",
                "")
    var_df <- data.frame(RTS, GAP, KTS, Gerdin)
    colnames(var_df) <- col_names
    ## Save as xtable
    make.and.save.xtable(var_df, file_name = "variable_table.tex", include_rownames = FALSE,
                         caption = "Model variables.", label = "variables_table")
}
