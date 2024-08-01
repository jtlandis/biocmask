

# group_by_SE <- function(.data, ...) {
#   .env <- rlang::caller_env()
#   .mask <- TidySEMaskManager$new(.data, .env, "group_by")
#   poke_ctx_local("SE:::mask_manager", .mask)
#   poke_ctx_local("SE:::dplyr_function", "group_by")
#   poke_ctx_local("SE:::caller_env", .env)
#   quos <- rlang::enquos(..., .named = TRUE)
#   nms <- names(quos)
#   for (k in seq_along(quos)) {
#     quo <- quos[[k]]
#     name <- nms[k]
#     .mask$eval_group_by_assays(quo, name)
#   }
#   .mask$finalize_group_by_data(.data)
# }


#' @importFrom dplyr bind_cols reframe across everything

expand_groups <- function(.rows, .cols) {
  .nrow <- nrow(.rows)
  .ncol <- nrow(.cols)
  dplyr::bind_cols(
    tidyr::nest(
      .rows,
      .row_keys = -c(.indices, .indices_group_id)
    ) |>
    dplyr::reframe(
      dplyr::across(
        dplyr::everything(),
        ~vec_rep(.x, times = .env$.ncol)
      ) |>
        dplyr::rename_with(.fn = \(x) gsub(".indices", ".rows", x = x))
    ),
    tidyr::nest(
      .cols,
      .col_keys = -c(.indices, .indices_group_id)
    ) |>
    dplyr::reframe(
      dplyr::across(
        dplyr::everything(),
        ~vec_rep_each(.x, times = .env$.nrow)
      ) |>
        dplyr::rename_with(.fn = \(x) gsub(".indices", ".cols", x = x))
    ),
    .name_repair = "minimal"
  ) |>
    dplyr::arrange(
      .rows_group_id,
      .cols_group_id
    ) |>
    dplyr::mutate(
      .group_id = 1:n()
    )
}

mat_index <- function(rows_ind, cols_ind, nrows) {
  shift <- (cols_ind - 1L) * nrows
  vctrs::vec_rep(rows_ind,
                 length(cols_ind)) +
    vctrs::vec_rep_each(shift,
                        length(rows_ind))
}

# vec_chop_assays <- function(.data, row_indices, col_indices) {
#   chops <- vctrs::vec_chop(as.vector(.data), indices = mat_index(row_indices, col_indices, nrow(.data)))
#   nrows <- purrr::map_int(row_indices, length)
#   ncols <- purrr::map_int(col_indices, length)
#   purrr::pmap(list(chops, nrows, ncols), ~ matrix(..1, ..2, ..3))
# }

# vec_chop_assays <- function(.data, row_indices, col_indices) {
#   purrr::map2(row_indices, col_indices,
#               function(.x, .y, .matrix) .matrix[.x, .y], .matrix = .data)
# }

vec_chop_assays <- function(.data, nest_indices) {
  nest_indices <- nest_indices |>
    tibble(x = _) |>
    tidyr::unnest(cols = x)
  purrr::map2(
    nest_indices$.rows, nest_indices$.cols,
    function(.x, .y, .data) .data[.x, .y], .data = .data
  )
}

vec_chop_assays_row <- function(.data, indices) UseMethod("vec_chop_assays_row")

vec_chop_assays_row.matrix <- function(.data, indices) {
  purrr::map(indices,
             function(.i, .data) .data[.i,,drop = TRUE],
             .data = .data)
}

vec_chop_assays_row.vctrs_grouped_list <- function(.data, indices) {
  if (length(.data)!=1) stop("reshaping by row expects a single")
  key <- attr(.data, ".keys")
  .data <- .data[[1]]
  purrr::map(
    indices,
    \(x) purrr::map(.data, ~ .x[x,,drop = T])
  ) |>
    new_grouped_lst(keys = key)
}

vec_chop_assays_col <- function(.data, indices) UseMethod("vec_chop_assays_col")

vec_chop_assays_col.matrix <- function(.data, indices) {
  purrr::map(indices,
             function(.i, .data) .data[,.i,drop = TRUE],
             .data = .data)
}

vec_chop_assays_col.vctrs_grouped_list <- function(.data, indices) {
  if (length(.data)!=1) stop("reshaping by row expects a single")
  key <- attr(.data, ".keys")
  .data <- .data[[1]]
  purrr::map(
    indices,
    \(x) purrr::map(.data, ~ .x[,x,drop = T])
  ) |>
    new_grouped_lst(keys = key)
}



create_groups <- function(.data, .rename = ".indices") {
  if (rlang::is_empty(.data)) return(NULL)
  .data |>
    tibble::as_tibble() |>
    vctrs::vec_group_loc() |>
    tidyr::unnest(key) |>
    dplyr::rename("{.rename}" := loc) |>
    dplyr::mutate("{.rename}_group_id" := 1:n())
}

biocmask_groups <- function(row_groups = NULL, col_groups = NULL) {
  out <- list(
    row_groups = create_groups(row_groups),
    col_groups = create_groups(col_groups)
  )
  type <- ""
  if (!is.null(row_groups)) {
    type <- "row"
  }
  if (!is.null(col_groups)) {
    type <- paste0(type, "col")
  }
  class(out) <- "biocmask_groups"
  attr(out, "type") <- type
  out
}

get_group_indices <- function(
    .groups,
    type = c("assays", "rowData", "colData")) {
  if (is.null(.groups)) return(NULL)
  type <- match.arg(type, c("assays", "rowData", "colData"))
  switch(
    type,
    assays = {
      out <- pull_group_indices(.groups)
      attr(out, "type") <- attr(.groups, "type")
      out},
    rowData = .groups$row_groups$.indices,
    colData = .groups$col_groups$.indices
  )
}

pull_group_indices <- function(.groups) {
  switch(
    group_type(.groups),
    rowcol = expand_groups(.groups$row_groups, .groups$col_groups),
    row = .groups$row_groups,
    col = .groups$col_groups
  )
}

group_type <- function(obj) {
  result <- attr(obj, "type")
  if (is.null(result)) return("none")
  result
}

`group_type<-` <- function(obj, value) {
  value <- match.arg(value, choices = c("rowcol", "row", "col"))
  attr(obj, "type") <- value
  obj
}

group_details <- function(obj) {
  group_data <- metadata(obj)[["group_data"]]
  row_groups <- group_data$row_groups %||% tibble(.indices = list(NULL), .indices_group_id = 1L)
  col_groups <- group_data$col_groups %||% tibble(.indices = list(NULL), .indices_group_id = 1L)
  out <- expand_groups(row_groups, col_groups)
  attr(out, "obj_dim") <- dim(obj)
  out
}

wrap <- function(obj) UseMethod("wrap")
wrap.vctrs_grouped_list <- function(obj) obj
wrap.default <- function(obj) list(obj)


#' Mutate a SummarizedExperiment object under an data mask
#' @param .data a SummarizedExperiment object
#' @param ... expressions
#' @value SummarizedExperiment object
#' @export
group_by.SummarizedExperiment <- function(.data, ...) {
  .env <- rlang::caller_env()
  mask <- new_biocmask.SummarizedExperiment(obj = .data)
  quos <- biocmask_quos(...)
  n_quo <- length(quos)
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "biocmask:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    rlang::abort(
      message = c(
       "x"="Cannot group in `assays` context",
        "*" = "grouping by assays is unsupported for SummarizedExperiment objects",
       "!" = sprintf("consider wrapping indices %s in rows(...) or cols(...)", 
                     paste0(err, collapse = ", "))
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
  groups <- biocmask_groups(
    row_groups = results$rows,
    col_groups = results$cols
  )
  metadata(.data)[["group_data"]] <- groups
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
