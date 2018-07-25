#' Generate flowchart vector function
#'
#' This function merges ns from an input result vector or list, and generates vector for flowchart tikz code. Number of excluded subjects are generated automatically.
#' @param results Numeric vector or numeric list. Results vector or list. No default.
#' @param node_text Character vector. Node text for each of ns in results. In the order in which ns is presented in the results list. No default.
#' @param exclusion_text Character vector. Length of node_text - 1. Node text for exclusion nodes. In the order in which ns is presenter in the results list. No default.
#' @param results_lst List. Results list, if such a list exist. Defaults to NULL.
#' @export
generate.flowchart.vec <- function(results, node_text, exclusion_text,
                                   results_lst = NULL){
    ## Error handling
    if (length(node_text) - 1 != length(exclusion_text)) stop("Exclusion criteria should be length of node_text -1")
    if (length(results) != length(node_text)) stop ("Length of node_text must match that of results.")
    ## Add space to exclusion text to adjust for node text
    exclusion_text <- c(exclusion_text, "")
    ## Generate numbers of excluded patients
    num_excl_patients <- sapply(seq_along(results), function (i){
        the_diff <- ""
        ## Calculate number of excluded patients
        if (i != length(results)) the_diff <- results[[i]] - results[[i + 1]]

        return (the_diff)
    })
    pair.function <- function(main_element, exclusion_element){
        ## Vectorize
        sapply(c(main_element, exclusion_element), function(element){
            element_with_quotes <- paste("\"", element, "\"")
            return (element_with_quotes)
        })
    }
    ## Define text and value lists
    value_lst <- list(main_element = results,
                     exclusion_element = num_excl_patients)
    text_lst <- list(main_element = node_text,
                       exclusion_element = exclusion_text)
    pair_lst <- lapply(setNames(list(value_lst, text_lst), nm = c("Value", "Text")),
                       function (lst){
                           ## Vectorize
                           vectorized <- unlist(mapply(pair.function,
                                                       main_element = lst$main_element,
                                                       exclusion_element = lst$exclusion_element,
                                                       SIMPLIFY = FALSE),
                                                recursive = FALSE)
                           ## Remove space vector element
                           return (head(vectorized, -1))
                       })
    if (!is.null(results_lst)) results$flowchart_lst <<- pair_lst

    return (pair_lst)
}
