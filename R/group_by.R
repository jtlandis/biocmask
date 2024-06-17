

group_by_SE <- function(.data, ...) {
  .env <- rlang::caller_env()
  .mask <- TidySEMaskManager$new(.data, .env, "group_by")
  poke_ctx_local("SE:::mask_manager", .mask)
  poke_ctx_local("SE:::dplyr_function", "group_by")
  poke_ctx_local("SE:::caller_env", .env)
  quos <- rlang::enquos(..., .named = TRUE)
  nms <- names(quos)
  for (k in seq_along(quos)) {
    quo <- quos[[k]]
    name <- nms[k]
    .mask$eval_group_by_assays(quo, name)
  }
  .mask$finalize_group_by_data(.data)
}


#' @importFrom dplyr bind_cols reframe across everything

expand_groups <- function(.rows, .cols) {
  .nrow <- nrow(.rows)
  .ncol <- nrow(.cols)
  dplyr::bind_cols(
    dplyr::reframe(
      .rows,
      dplyr::across(
        dplyr::everything(),
        vec_rep,
        times = .env$.ncol
      )
    ),
    dplyr::reframe(
      .cols,
      dplyr::across(
        dplyr::everything(),
        vec_rep_each,
        times = .env$.nrow
      )
    ),
    .name_repair = "minimal"
  ) |>
    dplyr::arrange(
      dplyr::pick(
        -c(.rows:(dplyr::last_col()))
      )
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

vec_chop_assays <- function(.data, row_indices, col_indices) {
  purrr::map2(row_indices, col_indices, 
              function(.x, .y, .matrix) .matrix[.x, .y], .matrix = .data)
}

vec_chop_assays_row <- function(.data, row_indices) {
  purrr::map(row_indices, function(.i, .matrix) .matrix[.i, ], .matrix = .data)
}

vec_chop_assays_col <- function(.data, col_indices) {
  purrr::map(col_indices, function(.i, .matrix) .matrix[, .i], .matrix = .data)
}
