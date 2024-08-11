

#' @export
arrange.SummarizedExperiment <- function(.data, ..., .by_group = FALSE) {
  .env <- caller_env()
  mask <- new_biocmask.SummarizedExperiment(obj = .data)
  poke_ctx_local("biocmask:::caller_env", .env)
  poke_ctx_local("biocmask:::manager", mask)
  poke_ctx_local("biocmask:::dplyr_verb", "arrange")
  quos <- biocmask_quos(...)
  if (.by_group) {
    quos <- c(biocmask_quos(!!!biocmask_curr_groups(.data)), quos)
  }
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
  type <- ""
  if (!is_empty(results$rows)) {
    type <- "row"
    ro <- exec("order", splice(results$rows))
  }
  
  if (!is_empty(results$cols)) {
    type <- paste0(type, "col")
    co <- exec("order", splice(results$cols))
  }
  
  out <- switch(type,
         rowcol = .data[ro, co],
         row = .data[ro,],
         col = .data[,co],
         .data)
  groups <- group_data(.data)
  if (!is.null(groups)) {
    if (!is_empty(groups$row_groups)) {
      group_inds <- group_ind(groups$row_groups$.indices, nrow(.data))
      new_id <- vctrs::vec_slice(group_inds, ro)
      new_grps <- vec_group_loc(new_id)
      inds <- vector('list', nrow(groups$row_groups))
      inds[new_grps$key] <- new_grps$loc
      groups$row_groups$.indices <- inds
    }
    
    if (!is_empty(groups$col_groups)) {
      group_inds <- group_ind(groups$col_groups$.indices, ncol(.data))
      new_id <- vctrs::vec_slice(group_inds, co)
      new_grps <- vec_group_loc(new_id)
      inds <- vector('list', nrow(groups$col_groups))
      inds[new_grps$key] <- new_grps$loc
      groups$col_groups$.indices <- inds
    }
    
    metadata(out)[["group_data"]] <- groups
  }
  
  out
}
