#' Generate analysis df function
#'
#' This function generatest the analysis df for xtable.maker.
#' @param analysis_lst List or data frame. Object containing point estimates as well as upper and lower boundary of ci. No default.
#' @param estimate_name String. Point stimate list label or df colname. Defaults to point_estimate.
#' @param lb_name String. Lower boundary element label or colname. Defaults to "lb"
#' @param ub_name String. Upper boundary label or colname. Defaults to "ub"
#' @export
generate.analysis.df <- function(analysis_lst,
                                 estimate_name = "point_estimate",
                                 lb_name = "lb",
                                 ub_name = "ub")
{
    analysis_lst <- analysis_lst$reclassification
    ## Error handling
    if (!(is.list(analysis_lst))) stop ("Analysis_lst must be data frame or list")
    if (!all(unlist(lapply(list(estimate_name, lb_name, ub_name), function (arg) length(arg) == 1)))) stop ("Args estimate_name, lb_name and ub_name should all be of length 1 ")
    ## Merge columns of analysis to parathesised cis
    analysis_objs <- lapply(analysis_lst, function (model_obj)
    {
        ## If list, coerce to data.frame
        if (!(is.data.frame(model_obj))){
            model_obj <- data.frame(point_estimate = model_obj$diff_point_estimate,
                                    t(model_obj$CI_diff),
                                    row.names = "ci")
        }
        ## Merge columns to paranthesised ci
        model_obj$ci <- paste(model_obj[, estimate_name],
                                               paste0("(",
                                                      paste(model_obj[, lb_name],
                                                            "to",
                                                            model_obj[, ub_name]),
                                                      ")"))
        ## Remove orgiginal col names
        model_obj[, c(estimate_name, lb_name, ub_name)] <- NULL

        return(model_obj)
    })
    ## Set colnames from of analysis_objs to model names
    analysis_objs <- lapply(setNames(nm = names(analysis_objs)),
                            function(obj_name) {
                                colnames(analysis_objs[[obj_name]]) <- obj_name
                                return (analysis_objs[[obj_name]])
                            }
                            )

    return (analysis_objs)
}
