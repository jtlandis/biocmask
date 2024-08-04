

#' @importFrom dplyr summarize summarise
NULL

#' @title Summarize SummarizedExperiment
#' @export
summarise.SummarizedExperiment <- function(.data, ...) {
  .env <- caller_env()
  .groups <- metadata(.data)[["group_data"]]
  mask <- new_biocmask.SummarizedExperiment(obj = .data)
  poke_ctx_local("biocmask:::caller_env", .env)
  poke_ctx_local("biocmask:::manager", mask)
  poke_ctx_local("biocmask:::dplyr_verb", "summarise")
  quos <- biocmask_quos(...)
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "biocmask:::ctx")
  if (! "assays" %in% ctxs) {
    abort(
      message = c(
        "No assay context expression detected",
        "!" = "at least one assay context expression required",
        "i" = "consider nesting data via `summarise(se, counts = list(counts))`"
      )
    )
  }
  nms  <- names(quos)
  mask <- biocmask_evaluate(mask, quos, ctxs, nms, .env)
  browser()
  assay_chops <- mask_pull_chops(mask$masks[["assays"]])
 
  group_vars_ <- group_vars(.data)
  row_data <- col_data <- NULL
  .nrow <- .ncol <- 1L
  if ((is_grouped <- is_grouped_rows(.groups)) || !is_empty(chops$rows)) {
    .nrow <- row_size <- nrow(.groups$row_groups) %||% 1L
    row_chops <- mask_pull_chops(
      mask$masks[["rows"]],
      union(group_vars_$row_groups, mask$masks[["rows"]]$added))
    row_chops[group_vars_$row_groups] <- map(
      row_chops[group_vars_$row_groups],
      function(group_vec) {
        map(group_vec, .subset, 1L)
      })
    enforce_chops_scalar(row_chops)
    row_data <- purrr::map(
      row_chops,
      vctrs::list_unchop
    ) |>
      as("DataFrame")
  }
  if (is_grouped_cols(.groups) || !is_empty(chops$cols)) {
    .ncol <- col_size <- nrow(.groups$col_groups) %||% 1L
    col_chops <- mask_pull_chops(
      mask$masks[["cols"]],
      union(group_vars_$col_groups, mask$masks[["cols"]]$added))
    col_chops[group_vars_$col_groups] <- map(
      col_chops[group_vars_$col_groups],
      function(group_vec) {
        map(group_vec, .subset, 1L)
      })
    enforce_chops_scalar(col_chops)
    col_data <- purrr::map(
      col_chops,
      vctrs::list_unchop
    ) |>
      as(Class = "DataFrame")
  }
  new_metadata <- metadata(.data)
  if (group_type(.groups) != "none") {
    new_metadata$group_data <- biocmask_groups(
      row_data[group_vars_$row_groups],
      col_data[group_vars_$col_groups])
  }
  
  
  #we should have some type of value to view from
  # assays as it was enforced earlier.
  assay_data <- purrr::map(
    assay_chops,
    vctrs::list_unchop
  ) |>
    purrr::map(
      matrix,
      nrow = .nrow,
      ncol = .ncol
    )
  
  SummarizedExperiment(assays = assay_data,
                       rowData = row_data,
                       colData = col_data,
                       metadata = new_metadata,
                       checkDimnames = FALSE)
}

#' @export
summarize.SummarizedExperiment <- summarise.SummarizedExperiment

enforce_chops_scalar <- function(chops) {
  purrr::iwalk(
    chops,
    function(vec, name) {
      purrr::walk(vec, vec_check_size, size = 1L, arg = name)
    }
  )
  invisible(NULL)
}

mask_pull_chops <- function(mask, names = NULL) {
  chop_env <- mask$environments@env_data_chop
  names <- names %||% mask$added
  names(names) <- names
  lapply(names, function(name) chop_env[[name]])
}

get_mask_chops <- function(mask) {
  lapply(mask$masks, mask_pull_chops)
}