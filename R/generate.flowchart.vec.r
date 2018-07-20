#' Generate flowchart vector function
#'
#' This function merges ns from an input result vector or list, and generates vector for flowchart tikz code. Number of excluded subjects are generated automatically.
#' @param results Numeric vector or numeric list. Results vector or list. No default.
#' @param node_text Character vector. Node text for each of ns in results. In the order in which ns is presented in the results list. No default.
#' @param exclusion_text Character vector. Length of node_text - 1. Node text for exclusion nodes. In the order in which ns is presenter in the results list. No default.
#' @param to_results Logical. If TRUE, save to results list. Defaults to FALSE.
#' @export
generate.flowchart.vec <- function(results, node_text, exclusion_text,
                                   to_results = FALSE){
    ## Error handling
    if (length(node_text) - 1 != length(exclusion_text)) stop("Exclusion criteria should be length of node_text -1")
    if (length(results) != length(node_text)) stop ("Length of node_text must match that of results.")
    ## Add space to exclusion text to match length of node text
    exclusion_text <- c(exclusion_text, "")
    ## Generate numbers of excluded patients
    num_excl_patients <- sapply(seq_along(results), function (i){
        the_diff <- ""
        ## Calculate number of excluded patients
        if (i != length(results)) the_diff <- results[[i]] - results[[i + 1]]

        return (the_diff)
    })
    ## Pair results ns with node_text
    pairs <- mapply(function(ns_element, a_node_text,
                             excl_element, a_excl_text){
        ## Pair for center nodes
        main_node_pair <- paste("\"", ns_element, a_node_text, "\"")
        ## Pair for exclusion nodes
        excl_node_pair <- paste("\"", excl_element, a_excl_text, "\"")
        ## Vectorize pairs
        paired_w_excl <- c(main_node_pair, excl_node_pair)

        return (paired_w_excl)
    }, results, node_text, num_excl_patients, exclusion_text,
    SIMPLIFY = FALSE)
    ## Vectorize pairs list and remove last, empty element
    vectorized_pair <- head(unlist(pairs), -1)
    if (to_results) results$ns_vec <<- vectorized_pair

    return (vectorized_pair)
}
