
#' @importFrom dplyr mutate

#' Mutate a SummarizedExperiment object under an data mask
#' @param .data a SummarizedExperiment object
#' @param ... expressions
#' @value SummarizedExperiment object
#' @export
mutate.SummarizedExperiment <- function(.data, ...) {
  # browser()
  .env <- rlang::caller_env()
  mask <- new_biocmask.SummarizedExperiment(obj = .data)
  quos <- biocmask_quos(...)
  n_quo <- length(quos)
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "biocmask:::ctx")
  nms  <- names(quos)
  results <- vector("list", n_quo)
  for(i in seq_len(n_quo)) {
    quo <- quos[[i]]
    nm <- nms[i]
    mask$ctx <- ctxs[[i]]
    mask$eval(quo, name = nm, env = .env)
  }
  results <- mask$results()
  nms <- names(results$assays)
  for (i in seq_along(results$assays)) {
    assays(.data, withDimnames = FALSE)[[nms[i]]] <- results$assays[[i]]
  }
  nms <- names(results$rows)
  for (i in seq_along(results$rows)) {
    rowData(.data)[[nms[i]]] <- results$rows[[i]]
  }
  nms <- names(results$cols)
  for (i in seq_along(results$cols)) {
    colData(.data)[[nms[i]]] <- results$cols[[i]]
  }
  .data
}

