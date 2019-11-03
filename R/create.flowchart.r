#' create.flowchart
#'
#' Wrapper for the bengaltiger::CreateFlowchart to make the code cleaner
#' @param results List. The results list. No default. 
#' @param ... Args for the bengaltiger::CreateFlowchart. 
#' @export
create.flowchart <- function(results, ...) {
    text <- c("patients were enroled in this study",
              "patients did not give informed consent",
              "patients gave informed consent",
              "patients had missing information",
              "patients had complete data",
              "patients were excluded from final analyses",
              "patients were included in final")
    ns <- unlist(results$n_s)
    ## Rolling differences
    differences <- abs(diff(ns))
    inclusions.and.exclusions <- unlist(lapply(seq_along(ns), function(i) {
        if (i == length(ns))
            return (ns[i])
        else
            return (list(ns[i], differences[i]))
    }))
    flowchart <-bengaltiger::CreateFlowchart(flowchart.elements = mapply(paste, inclusions.and.exclusions, text), 
                                             print.tikz = FALSE,
                                             read.from.results = FALSE,
                                             ...)
    return (flowchart)
}
 
