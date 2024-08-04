
#' @importFrom dplyr mutate

#' @title Mutate a SummarizedExperiment object
#' @name mutate
#' @description
#' Mutate a SummarizedExperiment object under an data mask
#' @param .data a SummarizedExperiment object
#' @param ... expressions
#' @return SummarizedExperiment object
#' @export
mutate.SummarizedExperiment <- function(.data, ...) {
  # browser()
  .env <- rlang::caller_env()
  mask <- new_biocmask.SummarizedExperiment(obj = .data)
  poke_ctx_local("biocmask:::caller_env", .env)
  poke_ctx_local("biocmask:::manager", mask)
  poke_ctx_local("biocmask:::dplyr_verb", "mutate")
  quos <- biocmask_quos(...)
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "biocmask:::ctx")
  nms  <- names(quos)
  mask <- biocmask_evaluate(mask, quos, ctxs, nms, .env, .matrix = TRUE)
  results <- mask$results()
  nms <- names(results$assays)
  for (i in seq_along(results$assays)) {
    assay(.data, nms[i], withDimnames = FALSE) <- results$assays[[i]]
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

