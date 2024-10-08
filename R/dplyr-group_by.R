

#' @title get grouping data
#' @name group_data-SummarizedExperiment
#' @description
#' retrieve grouping information from a `SummarizedExperiment` object. This
#' is stored within the `metadata()` of the object.
#' 
#' @param .data an object
#' @return list of groupings for an SummarizedExperiment
#' @examples
#' group_by(se_simple, rows(direction), cols(condition)) |> group_data()
#' @export
group_data.SummarizedExperiment <- function(.data) {
  metadata(.data)[["group_data"]]
}

#' @title apply groups to SummarizedExperiments
#' @description
#' create grouping variables about the rowData and colData of a 
#' SummarizedExperiment object. Unlike the `data.frame` method
#' the resulting output class is left unchanged. Thus `dplyr` generics for
#' `SummarizedExperiment` must check grouping information manually.
#' @param .data a SummarizedExperiment object
#' @param ... expressions to group on. Grouping may only be done on
#' rowData and/or colData by `rows()` and `cols()` respectively.
#' @param .add When `FALSE`, the default, `group_by()` will override
#' existing groups.
#' @return SummarizedExperiment object
#' @examples
#' 
#' group_by(se_simple, rows(direction), cols(condition))
#' 
#' @export
group_by.SummarizedExperiment <- function(.data, ..., .add = FALSE) {
  # browser()
  .env <- caller_env()
  # to maintain consistency with dplyr
  # force any computations to occur on ungrouped data
  .groups <- metadata(.data)[["group_data"]]
  metadata(.data)[["group_data"]] <- NULL
  mask <- new_biocmask_manager.SummarizedExperiment(obj = .data)
  poke_ctx_local("biocmask:::caller_env", .env)
  poke_ctx_local("biocmask:::manager", mask)
  poke_ctx_local("biocmask:::dplyr_verb", "group_by")
  quos <- biocmask_quos(..., .ctx_default = "assays", .ctx_opt = c("rows", "cols"))
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "biocmask:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    abort(
      message = c(
       "Cannot group in `assays` context",
       "x" = sprintf("review expression indices %s in dots",
                     paste0(which(err), collapse = ", ")),
       "i" = "consider wrapping expressions in rows(...) or cols(...)"
      )
    )
  }
  nms  <- names(quos)
  mask <- biocmask_evaluate(mask, quos, ctxs, nms, .env)
  results <- mask$results()
  # nms <- names(results$assays)
  # for (i in seq_along(results$assays)) {
  #   assays(.data, withDimnames = FALSE)[[nms[i]]] <- results$assays[[i]]
  # }
  
  if (.add && !is_empty(curr_groups <- metadata(.data)[["group_data"]])) {
    if (!is_empty(curr_groups$row_groups)) {
      curr <- select(curr_groups$row_groups, - starts_with(".indices")) |>
        names()
      curr <- rowData(.data)[curr]
      curr[names(results$rows)] <- results$rows
      results$rows <- curr
    }
    if (!is_empty(curr_groups$col_groups)) {
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
  if (".features" %in% nms) {
    rownames(.data) <- results$rows$.features
    results$rows$.features <- NULL
    nms <- names(results$rows)
  }
  if (length(nms)) {
    row_data <- rowData(.data)
    row_data[nms] <- results$rows
    #push nms to the front
    row_data <- row_data[c(nms, setdiff(names(row_data), nms))]
    rowData(.data) <- row_data
  }
  
  # for (i in seq_along(results$rows)) {
  #   rowData(.data)[[nms[i]]] <- results$rows[[i]]
  # }
  nms <- names(results$cols)
  if (".samples" %in% nms) {
    colnames(.data) <- results$cols$.samples
    results$cols$.samples <- NULL
    nms <- names(results$cols)
  }
  if (length(nms)) {
    col_data <- colData(.data)
    col_data[nms] <- results$cols
    #push nms to the front
    col_data <- col_data[c(nms, setdiff(names(col_data), nms))]
    colData(.data) <- col_data
  }
  # for (i in seq_along(results$cols)) {
  #   colData(.data)[[nms[i]]] <- results$cols[[i]]
  # }
  .data
}


#' @describeIn group_by.SummarizedExperiment Ungroup a SummarizedExperiment object
#' 
#' @param x A SummarizedExperiment object
#' @param ... [contextual expressions][biocmask::biocmask-context] specifying 
#' which columns to ungroup. Omitting `...` ungroups the entire object.
#' @export
ungroup.SummarizedExperiment <- function(x, ...) {
  quos <- biocmask_quos(..., .named = FALSE, .ctx_default = "assays",
                        .ctx_opt = c("rows", "cols"))
  curr_groups <- metadata(x)[["group_data"]]
  if (is_empty(curr_groups)) return(x)
  n_quo <- length(quos)
  if (n_quo==0L) {
    metadata(x)["group_data"] <- NULL
    return(x)
  }
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "biocmask:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    abort(
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
  if (!is_empty(by_ctx$rows)) {
    select(curr_groups$row_groups, - starts_with(".indices")) |>
      names()
    select_expr <- call2("c", splice(by_ctx$rows))
    to_remove <- eval_select(
      select_expr, 
      data = as.list(colData(x)),
      allow_rename = FALSE)
    to_remove <- names(to_remove)
    new_groups <- setdiff(old_groups, to_remove)
    update_rows <- call2("rows", splice(syms(new_groups)))
    update_ <- "row"
  }
  if (!is_empty(by_ctx$cols)) {
    old_groups <- select(curr_groups$col_groups, - starts_with(".indices")) |>
      names()
    select_expr <- call2("c", splice(by_ctx$cols))
    to_remove <- eval_select(
      select_expr, 
      data = as.list(colData(x)),
      allow_rename = FALSE)
    to_remove <- names(to_remove)
    new_groups <- setdiff(old_groups, to_remove)
    update_cols <- call2("cols", splice(syms(new_groups)))
    update_ <- paste0(update_, "col")
  }
  switch(update_,
         rowcol = group_by(x, !!update_rows, !!update_cols),
         row = group_by(x, !!update_rows),
         col = group_by(x, !!update_cols))
}



#' @export
groups.SummarizedExperiment <- function(x) {
  map(
    group_vars(x),
    syms
  )
}

biocmask_curr_groups <- function(x) {
  vars <- groups(x)
  row_v <- if (is_empty(vars$row_groups)) {
    NULL
  } else {
    call2("rows", splice(vars$row_groups))
  }
  col_v <- if (is_empty(vars$col_groups)) {
    NULL
  } else {
    call2("cols", splice(vars$col_groups))
  }
  c(row_v, col_v)
}
