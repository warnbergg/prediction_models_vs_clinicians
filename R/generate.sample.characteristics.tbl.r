#' Create table of sample characteristics function
#'
#' This function generates the table of sample characteristics
#' @param data_for_table The data to use to create the table. No default.
#' @param data_dictionary The data dictionary. No default
#' @param strata The strata variable, defaults to NULL.
#' @param vars The variables to include in the table, defaults to NULL, in which case it is defined as names(data_dictionary)[sapply(data_dictionary, function(x) x$incl == "Yes")].
#' @param exclude_vars Character vector of variable names to exclude from table, defaults to NULL.
#' @param include_overall Logical and used only if strata is not NULL. defaults to TRUE in which case an overall column is also included.
#' @param include_missing_column Logical. If TRUE a column is missing indicating the number (%) of missing values in each variable. Details to TRUE.
#' @param digits Integer. Number of digits to use when rounding table entries. Defaults to 1.
#' @param save Logical. If TRUE the table object is also saved to disk as a .tex file. Defaults to FALSE.
#' @export
generate.sample.characteristics.tbl <- function(
                                                data_for_table,
                                                data_dictionary,
                                                strata = NULL,
                                                vars = NULL,
                                                exclude_vars = NULL,
                                                include_overall = TRUE,
                                                include_missing_column = TRUE,
                                                digits = 1,
                                                save = FALSE
                                                )
{
## create table data frame
table_data <- all[, names(varlist)]
## set colnames using the varlist
colnames(table_data) <- unlist(lapply(varlist, function(x) x$label))
## replace integers with value labels
table_data[] <- lapply(varlist, function(x) {
    v <- table_data[[x$label]]
    if (!is.null(x$levels)) {
        n <- names(x$levels)
        for (i in seq_along(n)) v[v == n[i]] <- x$levels[[i]]
        v <- as.factor(v)
    }
    return(v)
})
## get complete cases
complete <- table_data[complete.cases(table_data), ]
## add strata variable
#complete$case <- "Complete cases"
#table_data$case <- "All cases"
### merge complete and all cases
#table_data <- rbind(complete, table_data)
#table_data$case <- as.factor(table_data$case)
## the full names cause problems with tableone, so back to abbreviations
colnames(complete) <- names(varlist)
print(CreateTableOne(data = t.NRI.rts))
## create table one
table <- print(CreateTableOne(data = complete,
                              strata = "s30d",
                              includeNA = TRUE,
                              test = FALSE),
               showAllLevels = TRUE,
               noSpaces = TRUE,
               catDigits = 0,
               contDigits = 0,
               printToggle = FALSE,
               nonnormal = unlist(lapply(names(varlist), function(x) if (is.null(varlist[[x]]$levels)) x)))
moi_position <- c(grep("moi", row.names(table)):nrow(table))
moi_for_sort <- table[moi_position,]
sorted_moi <- moi_for_sort[rev(mixedorder(moi_for_sort[, "Survivors"])),]
row.names(sorted_moi) <- c("moi (%)", rep("", nrow(sorted_moi) - 1))
table <- rbind(table[-moi_position,], sorted_moi)
## put rownames as column
table <- cbind(rownames(table), table)
rownames(table) <- 1:nrow(table)
## add colname and capitalise level
colnames(table)[1:2] <- c("Characteristic", "Level")
## replace short names with full names
first_column <- as.character(table[, 1])
n <- names(varlist)
for (i in seq_along(n)) first_column <- gsub(paste0("^", n[i], " "),
                                             paste0(varlist[[n[i]]]$label, " "),
                                             first_column)
table[,1] <- first_column
## remove s30d rows from table
case <- grep("Dead", first_column)
if (first_column[case + 1] == "") case <- c(case, case + 1)
table <- table[-case, ]
## replace NA with Missing
table[is.na(table)] <- "Missing"
## arrange columns in logical order
table <- table[, c(1, 2, 4, 3)]
## frequency (%) of survivors and non-survivors for Table1
freq <- table[1, c("Survivors", "Non-Survivors")]
percent <- sapply(freq, function(x) round(as.numeric(x)/sum(as.numeric(freq)),
                                          digits = 2))
## descriptive statistics
male <- table(mdfm$sex)[names(table(mdfm$sex)) == "1"]
male_pct <- round(male/length(mdfm$sex), digits = 2)
most_moi <- round(max(table(mdfm$moi))/sum(table(mdfm$moi)),
                  digits = 2)
## create xtable
caption <- paste("Sample characteristics. Presented as median (IQR) or frequency (\\%).",
                 "Abbreviations: HR Heart rate, GCS Glasgow coma scale, ",
                 "RR Respiratory rate, SBP Systolic blood pressure.")

xtable <- print.xtable(xtable(table,
                              caption = caption,
                              align = rep("X", ncol(table) +1)),
                       tabular.environment = "tabularx",
                       floating = TRUE,
                       width = "\\textwidth",
                       table.placement = "H",
                       caption.placement = "top",
                       include.rownames = FALSE,
                       booktabs = TRUE,
                       print.results = FALSE)
}
