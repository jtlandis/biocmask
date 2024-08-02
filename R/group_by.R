


#' Mutate a SummarizedExperiment object under an data mask
#' @param .data a SummarizedExperiment object
#' @param ... expressions
#' @value SummarizedExperiment object
#' @export
group_by.SummarizedExperiment <- function(.data, ..., .add = FALSE) {
  # browser()
  .env <- rlang::caller_env()
  mask <- new_biocmask.SummarizedExperiment(obj = .data)
  quos <- biocmask_quos(...)
  n_quo <- length(quos)
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "biocmask:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    rlang::abort(
      message = c(
       "Cannot group in `assays` context",
       "x" = sprintf("review expression indices %s in dots",
                     paste0(which(err), collapse = ", ")),
       "i" = "consider wrapping expressions in rows(...) or cols(...)"
      )
    )
  }
  nms  <- names(quos)
  results <- vector("list", n_quo)
  for(i in seq_len(n_quo)) {
    quo <- quos[[i]]
    nm <- nms[i]
    mask$ctx <- ctxs[[i]]
    mask$eval(quo, name = nm, env = .env)
  }
  results <- mask$results()
  # nms <- names(results$assays)
  # for (i in seq_along(results$assays)) {
  #   assays(.data, withDimnames = FALSE)[[nms[i]]] <- results$assays[[i]]
  # }
  if (.add) {
    curr_groups <- metadata(.data)[["group_data"]]
    if (rlang::is_empty(curr_groups)) break # do nothing
    if (!rlang::is_empty(curr_groups$row_groups)) {
      curr <- select(curr_groups$row_groups, - starts_with(".indices")) |>
        names()
      curr <- rowData(.data)[curr]
      curr[names(results$rows)] <- results$rows
      results$rows <- curr
    }
    if (!rlang::is_empty(curr_groups$col_groups)) {
      curr <- select(curr_groups$col_groups, - starts_with(".indices")) |>
        names()
      curr <- colData(.data)[curr]
      curr[names(results$cols)] <- results$cols
      results$rows <- curr
    }
  }
  groups <- biocmask_groups(
    row_groups = results$rows,
    col_groups = results$cols
  )
  metadata(.data)[["group_data"]] <- groups
  nms <- names(results$rows)
  rowData(.data)[nms] <- results$rows
  # for (i in seq_along(results$rows)) {
  #   rowData(.data)[[nms[i]]] <- results$rows[[i]]
  # }
  nms <- names(results$cols)
  colData(.data)[nms] <- results$cols
  # for (i in seq_along(results$cols)) {
  #   colData(.data)[[nms[i]]] <- results$cols[[i]]
  # }
  .data
}


ungroup.SummarizedExperiment <- function(x, ...) {
  quos <- biocmask_quos(..., .named = FALSE)
  curr_groups <- metadata(x)[["group_data"]]
  if (rlang::is_empty(curr_groups)) return(x)
  n_quo <- length(quos)
  if (n_quo==0L) {
    metadata(x)[["group_data"]] <- NULL
    return(x)
  }
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "biocmask:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    rlang::abort(
      message = c(
        "Cannot ungroup in `assays` context",
        "x" = sprintf("review expression indices %s in dots",
                      paste0(which(err), collapse = ", ")),
        "i" = "consider wrapping expressions in rows(...) or cols(...)"
      )
    )
  }
  by_ctx <- split(quos, ctxs)
  update_cols <- update_rows <- NULL
  update_ <- ""
  if (!rlang::is_empty(by_ctx$rows)) {
    select(curr_groups$row_groups, - starts_with(".indices")) |>
      names()
    select_expr <- rlang::call2("c", splice(by_ctx$rows))
    to_remove <- tidyselect::eval_select(
      select_expr, 
      data = as.list(colData(x)),
      allow_rename = FALSE)
    to_remove <- names(to_remove)
    new_groups <- setdiff(old_groups, to_remove)
    update_rows <- rlang::call2("rows", splice(syms(new_groups)))
    update_ <- "row"
  }
  if (!rlang::is_empty(by_ctx$cols)) {
    old_groups <- select(curr_groups$col_groups, - starts_with(".indices")) |>
      names()
    select_expr <- rlang::call2("c", splice(by_ctx$cols))
    to_remove <- tidyselect::eval_select(
      select_expr, 
      data = as.list(colData(x)),
      allow_rename = FALSE)
    to_remove <- names(to_remove)
    new_groups <- setdiff(old_groups, to_remove)
    update_cols <- rlang::call2("cols", splice(syms(new_groups)))
    update_ <- paste0(update_, "col")
  }
  switch(update_,
         rowcol = group_by(x, !!update_rows, !!update_cols),
         row = group_by(x, !!update_rows),
         col = group_by(x, !!update_cols))
}
