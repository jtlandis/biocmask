
#' @title arrange rows or columns
#' @description
#' `arrange()` orders either the rows or columns of a `SummarizedExperiment`
#' object. Note, to guarentee a valid `SummarizedExperiment` is returned, 
#' arranging in the `assays` evaluation context is disabled.
#' 
#' Unlike other dplyr verbs, `arrange()` largely ignores grouping. The 
#' `SummarizedExperiment` method also provides the same functionality via the
#' `.by_group` argument.
#' 
#' @inheritParams dplyr::arrange
#' @return an object inheriting SummarizedExperiment class
#' @examples
#' 
#' #arrange within rows/cols contexts separately
#' arrange(se_simple,
#'         rows(direction),
#'         cols(dplyr::desc(condition)))
#' 
#' # access assay data to compute arrangement
#' arrange(se_simple, 
#'         rows(rowSums(.assays_asis$counts)),
#'         cols(colSums(.assays_asis$counts)))
#' 
#' # assay context is disabled
#' arrange(se_simple, counts) |> try()
#' 
#' # convert to `data.frame` first
#' as.data.frame(se_simple) |>
#'   arrange(counts)
#' 
#' 
#' @export
arrange.SummarizedExperiment <- function(.data, ..., .by_group = FALSE) {
  .env <- caller_env()
  quos <- biocmask_quos(..., .ctx_default = "assays",
                        .ctx_opt = c("rows", "cols"))
  if (.by_group) {
    quos <- c(biocmask_quos(!!!biocmask_curr_groups(.data),
                            .ctx_default = "assays",
                            .ctx_opt = c("rows", "cols")), quos)
  }
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "biocmask:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    abort(
      message = c(
        "Cannot arrange in `assays` context",
        "x" = sprintf("review expression indices %s in dots",
                      paste0(which(err), collapse = ", ")),
        "i" = "consider wrapping expressions in rows(...) or cols(...)"
      )
    )
  }
  nms  <- names(quos)
  # to make this function consistent
  groups <- group_data(.data)
  metadata(.data)[["group_data"]] <- NULL
  mask <- new_biocmask_manager.SummarizedExperiment(obj = .data)
  poke_ctx_local("biocmask:::caller_env", .env)
  poke_ctx_local("biocmask:::manager", mask)
  poke_ctx_local("biocmask:::dplyr_verb", "arrange")
  mask <- biocmask_evaluate(mask, quos, ctxs, nms, .env)
  results <- mask$results()
  type <- ""
  if (!is_empty(results$rows)) {
    type <- "row"
    ro <- exec("order", splice(results$rows), method = "radix")
  }
  
  if (!is_empty(results$cols)) {
    type <- paste0(type, "col")
    co <- exec("order", splice(results$cols), method = "radix")
  }
  
  out <- switch(type,
         rowcol = .data[ro, co],
         row = .data[ro,],
         col = .data[,co],
         .data)
  
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
