# Meant to be used to propagate NULL when a downstream
# function could take NULLs
`%|!|%` <- function(x, y) if(!is_empty(x)) y else NULL

#' @title Filter SummarizedExperiment
#' @description
#' Filtering operations can only return a SummarizedExperiment
#' object so long as they are performed on rows and cols contexts.
#' This is because filtering on assays does not gaurentee a recoverable
#' SummarizedExperiment structure.
#' 
#' @param .data A SummarizedExperiment object
#' @param ... conditions to filter on. These must be wrapped in `cols()` and or
#' `rows()`
#' @param .preserve Relevant when the .data input is grouped. If .preserve = FALSE
#' (the default), the grouping structure is recalculated based on the resulting data,
#' i.e. the number of groups may change.
#' @return SummarizedExperiment Object
#' @export
filter.SummarizedExperiment <- function(.data, ..., .preserve = FALSE) {
  .env <- caller_env()
  .groups <- metadata(.data)[["group_data"]]
  mask <- new_biocmask.SummarizedExperiment(obj = .data)
  poke_ctx_local("biocmask:::caller_env", .env)
  poke_ctx_local("biocmask:::manager", mask)
  poke_ctx_local("biocmask:::dplyr_verb", "filter")
  quos <- biocmask_quos(...)
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "biocmask:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    abort(
      message = c(
        "Cannot filter in `assays` context",
        "x" = sprintf("review expression indices %s in dots",
                      paste0(which(err), collapse = ", ")),
        "i" = "consider wrapping expressions in rows(...) or cols(...)"
      )
    )
  }
  nms  <- names(quos)
  mask <- biocmask_evaluate(mask, quos, ctxs, nms, .env)
  results <- mask$results()
  filter_ <- ""
  if (!is_empty(results$rows)) {
    row_logic <- vec_recycle_common(splice(results$rows)) |>
      purrr::reduce(`&`)
    filter_ <- "row"
  }
  if (!is_empty(results$cols)) {
    col_logic <- vec_recycle_common(splice(results$cols)) |>
      purrr::reduce(`&`)
    filter_ <- paste0(filter_, "col")
  }
  .data <- switch(
    filter_,
    rowcol = .data[row_logic, col_logic],
    row = .data[row_logic, ],
    col = .data[, col_logic],
    .data
  )
  current_groups <- metadata(.data)[["group_data"]]
  if (is.null(current_groups)) return(.data)
  row_select <- grep("^.indices", names(current_groups[["row_groups"]]),
                     value = TRUE, invert = TRUE)
  row_groups <- row_select %|!|% rowData(.data)[row_select]
  col_select <- grep("^.indices", names(current_groups[["col_groups"]]),
                     value = TRUE, invert = TRUE)
  col_groups <- col_select %|!|% colData(.data)[col_select]
  new_groups <- biocmask_groups(
    row_groups = row_groups,
    col_groups = col_groups
  )
  if (.preserve) {
    if (!is_empty(current_groups$row_groups)) {
      current_groups$row_groups$.indices[] <- list(integer())
      new_groups$row_groups <- rows_update(
        current_groups$row_groups,
        new_groups$row_groups,
        by = row_select
      )
    }
    if (!is_empty(current_groups$col_groups)) {
      current_groups$col_groups$.indices[] <- list(integer())
      new_groups$col_groups <- rows_update(
        current_groups$col_groups,
        new_groups$col_groups,
        by = col_select
      )
    }
  }
  metadata(.data)[["group_data"]] <- new_groups
  .data
}