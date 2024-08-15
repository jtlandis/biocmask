


select.SummarizedExperiment <- function(.data, ...) {
  
  .env <- caller_env()
  poke_ctx_local("biocmask:::caller_env", .env)
  poke_ctx_local("biocmask:::dplyr_verb", "select")
  quos <- biocmask_quos(...)
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "biocmask:::ctx")
  nms  <- names(quos)
  
  
  selected <- biocmask_eval_select(
    quos = quos, ctxs = ctxs, data = .data
  )
  
  
  assays(.data) <- assays(.data)[selected[["assays"]]]
  rowData(.data) <- rowData(.data)[selected[["rows"]]]
  colData(.data) <- colData(.data)[selected[["cols"]]]
  
  .data
  
}


biocmask_eval_select <- function(quos, ctxs, data) {
  
  out <- vector("list", length(quos))
  .data <- list(
    assays = as.list(assays(data)),
    rows = as.list(rowData(data)),
    cols = as.list(colData(data))
  )
  
  for (i in seq_along(quos)) {
    quo <- quos[[i]]
    ctx <- ctxs[i]
    out[[i]] <- tidyselect::eval_select(quo, .data[[ctx]])
  }
  
  ctxs <- factor(ctxs, levels = c("assays","rows","cols"))
  split(out, ctxs) |>
    map(function(.x) {
      unlist(.x) |>
        unique()
    })
  
}

