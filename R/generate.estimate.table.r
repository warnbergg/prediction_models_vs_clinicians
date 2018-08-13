#' Generate analysis df function
#'
#' This function generatest the analysis df for xtable.maker.
#' @param analysis_lst List or data frame. Object containing point estimates as well as upper and lower boundary of ci. No default.
#' @param pretty_names Character vector. Pretty names of models. No default.
#' @param man_estimate_labels Character vector. Optional names for estimates. Defaults to NULL.
#' @param estimate_name String. Point estimate list label or df colname. Defaults to point_estimate.
#' @param lb_name String. Lower boundary element label or colname. Defaults to "lb"
#' @param ub_name String. Upper boundary label or colname. Defaults to "ub"
#' @export
generate.estimate.table <- function(analysis_lst, pretty_names,
                                    man_estimate_labels = NULL,
                                    estimate_name = "point_estimate",
                                    lb_name = "lb",
                                    ub_name = "ub")
{
    ## Error handling
    ## Must be list or data frame,
    if (!(is.list(analysis_lst))) stop ("Analysis_lst must be data frame or list")
    ## Check length of arguments
    if (!all(unlist(lapply(list(estimate_name, lb_name, ub_name), function (arg) length(arg) == 1)))) stop ("Args estimate_name, lb_name and ub_name should all be of length 1 ")
    ## Check equal lengths of names and list elements
    if (length(names(analysis_lst)) != length(pretty_names)) stop ("Input pretty names the same length as analysis_lst elements names")
    ## Merge columns of analysis to parathesised cis
    analysis_objs <- mapply(function (model_obj, pretty_name){
        ## If list, coerce to data.frame
        if (!(is.data.frame(model_obj))){
            model_obj <- data.frame(point_estimate = model_obj$diff_point_estimate,
                                    t(model_obj$CI_diff),
                                    row.names = "ci")
        }
        ## Merge columns to paranthesised ci
        model_obj[, pretty_name] <- paste(model_obj[, estimate_name],
                                          paste0("(",
                                                 paste(model_obj[, lb_name],
                                                       "to",
                                                       model_obj[, ub_name]),
                                                 ")"))
        ## Remove orgiginal col names
        model_obj[, c(estimate_name, lb_name, ub_name)] <- NULL

        return(model_obj)
    },
    analysis_lst, pretty_names, SIMPLIFY = FALSE)
    ## Merge analysis object to table
    merged <- do.call(cbind, analysis_objs)
    ## Change row names
    if (!is.null(man_estimate_labels)){
         if (length(man_estimate_labels) == nrow(merged)){
             rownames(merged) <- man_estimate_labels
         } else stop ("Manual estimate labels must be same length as input estimates.")
    }
    ## Subscript con and cat in colnames which have underscore
    undscr_names <- grep("_", colnames(merged))
    ## Sub underscores for textsubscripts
    colnames(merged)[undscr_names] <- paste0(gsub("_",
                                                  "\\\\textsubscript{",
                                                  colnames(merged)[undscr_names]),
                                             "}")

    return (merged)
}
