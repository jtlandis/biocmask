

#' @title select assays, rowData, and colData names
#' @description
#' Select one or more values from each context. By default omitting an expression
#' for a context is the same as selecting NOTHING from that context. 
#' 
#' The <[`tidy-select`][dplyr::dplyr_tidy_select]> implementation within 
#' `biocmask` is almost similar to `dplyr` except that the data provided to
#' [eval_select][tidyselect::eval_select] is a zero length slice of the data.
#' This was an intentional choice to prevent the evaluation of potentionally 
#' expensive chopping operations for S4Vectors. This means that predicate
#' function from [`where()`][tidyselect::where] will NOT be able to query the
#' original data.
#' 
#' 
#' @param .data a `SummarizedExperiment` object
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> one or more selection 
#' expressions. Supports the `SummarizedExperiment-context-functions`.
#' @export
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

# underlying selection function. This may need to be an S3 generic
# for when `biocmask` extends to other classes
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

