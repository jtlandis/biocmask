# whenever new row binding is a
connect_assays_to_rows <- function(mask_assays, mask_rows) {
  assay_names <- mask_assays$names
  size <- length(assay_names) + 20L
  env_mask_bind <- mask_rows$environments[[1]] #last level
  env_asis <- new.env(hash = TRUE, parent = env_mask_bind, size = size)
  env_pronoun <- new.env(hash = TRUE, parent = env_asis, size = size)
  fun_asis <- add_bind(
    quote(do.call("cbind", .subset(!!name_sym, `biocmask:::assays:::current_chops`))),
    # should be evaluated within the chopped context, cannot guarantee groupings
    .env_expr = mask_assays$environments@env_data_chop,
    .env_bind = env_asis,
    type = "active"
  )
  fun_mold <- add_bind(
    quote(lapply(1:`biocmask:::ctx:::n`, function(i,x) x[i,,drop=TRUE], x = !!name_sym)),
    .env_expr = env_asis,
    .env_bind = env_pronoun,
    type = "active"
  )
  purrr::walk(assay_names, fun_asis) |>
    purrr::walk(fun_mold)
  # whenever a new assay binding is made,
  # execute the prior functions
  mask_assays$on_bind(fun_asis)$on_bind(fun_mold)
  # allow assay data to be seen from rowData context
  mask_rows$environments@env_foreign_data |>
    env_bind(
      .assays_asis = as_data_pronoun(env_asis),
      .assays = as_data_pronoun(env_pronoun)
    )
  invisible(env_pronoun)
}
connect_assays_to_cols <- function(mask_assays, mask_cols) {
  assay_names <- mask_assays$names
  size <- length(assay_names) + 20L
  env_mask_bind <- mask_cols$environments[[1]] #last level
  env_asis <- new.env(hash = TRUE, parent = env_mask_bind, size = size)
  env_pronoun <- new.env(hash = TRUE, parent = env_asis, size = size)
  fun_asis <- add_bind(
    quote(do.call("rbind", .subset(!!name_sym, `biocmask:::assays:::current_chops`))),
    # should be evaluated within the chopped context, cannot guarantee groupings
    .env_expr = mask_assays$environments@env_data_chop,
    .env_bind = env_asis,
    type = "active"
  )
  fun_mold <- add_bind(
    quote(lapply(1:`biocmask:::ctx:::n`, function(i,x) x[,i,drop=TRUE], x = !!name_sym)),
    .env_expr = env_asis,
    .env_bind = env_pronoun,
    type = "active"
  )
  purrr::walk(assay_names, fun_asis) |>
    purrr::walk(fun_mold)
  # whenever a new assay binding is made,
  # execute the prior functions
  mask_assays$on_bind(fun_asis)$on_bind(fun_mold)
  # allow assay data to be seen from rowData context
  mask_cols$environments@env_foreign_data |>
    env_bind(
      .assays_asis = as_data_pronoun(env_asis),
      .assays = as_data_pronoun(env_pronoun)
    )
  invisible(env_pronoun)
}

connect_rows_to_assays <- function(mask_rows, mask_assays) {
  row_names <- mask_rows$names
  size <- length(row_names) + 20L
  env_mask_bind <- mask_assays$environments[[1]] #last level
  env_asis <- new.env(hash = TRUE, parent = env_mask_bind, size = size)
  env_pronoun <- new.env(hash = TRUE, parent = env_asis, size = size)
  # assays is always the "richer" grouping, we can guarantee
  # that the current_chops scope has a scalar value
  fun_asis <- add_bind(
    quote(.subset2(!!name_sym, `biocmask:::rows:::current_chops`)),
    # should be evaluated within the chopped context, cannot guarantee groupings
    .env_expr = mask_rows$environments@env_data_chop,
    .env_bind = env_asis,
    type = "active"
  )
  fun_mold <- add_bind(
    quote(vec_rep(!!name_sym, times = `biocmask:::ctx:::ncol`)),
    .env_expr = env_asis,
    .env_bind = env_pronoun,
    type = "active"
  )
  purrr::walk(row_names, fun_asis) |>
    purrr::walk(fun_mold)
  # whenever a new row binding is made,
  # execute the prior functions
  mask_rows$on_bind(fun_asis)$on_bind(fun_mold)
  # allow row data to be seen from assay context
  mask_assays$environments@env_foreign_data |>
    env_bind(
      .rows_asis = as_data_pronoun(env_asis),
      .rows = as_data_pronoun(env_pronoun)
    )
  invisible(env_pronoun)
}

connect_cols_to_assays <- function(mask_cols, mask_assays) {
  col_names <- mask_cols$names
  size <- length(col_names) + 20L
  env_mask_bind <- mask_assays$environments[[1]] #last level
  env_asis <- new.env(hash = TRUE, parent = env_mask_bind, size = size)
  env_pronoun <- new.env(hash = TRUE, parent = env_asis, size = size)
  # assays is always the "richer" grouping, we can guarantee
  # that the current_chops scope has a scalar value
  fun_asis <- add_bind(
    quote(.subset2(!!name_sym, `biocmask:::cols:::current_chops`)),
    .env_expr = mask_cols$environments@env_data_chop,
    .env_bind = env_asis,
    type = "active"
  )
  fun_mold <- add_bind(
    quote(vec_rep_each(!!name_sym, times = `biocmask:::ctx:::nrow`)),
    .env_expr = env_asis,
    .env_bind = env_pronoun,
    type = "active"
  )
  purrr::walk(col_names, fun_asis) |>
    purrr::walk(fun_mold)
  # whenever a new col binding is made,
  # execute the prior functions
  mask_cols$on_bind(fun_asis)$on_bind(fun_mold)
  # allow col data to be seen from assay context
  mask_assays$environments@env_foreign_data |>
    env_bind(
      .cols_asis = as_data_pronoun(env_asis),
      .cols = as_data_pronoun(env_pronoun)
    )
  invisible(env_pronoun)
}

connect_rows_to_cols <- function(mask_rows, mask_cols) {
  row_names <- mask_rows$names
  size <- length(row_names) + 20L
  env_mask_bind <- mask_cols$environments[[1]] #last level
  env_asis <- new.env(hash = TRUE, parent = env_mask_bind, size = size)
  env_pronoun <- new.env(hash = TRUE, parent = env_asis, size = size)
  # bind
  fun_asis <- add_bind(
    # row data may be grouped. use vctrs::vec_c to concatenate vectors
    quote(vec_c(splice(.subset(!!name_sym, `biocmask:::rows:::current_chops`)))),
    .env_expr = mask_rows$environments@env_data_chop,
    .env_bind = env_asis,
    type = "active"
  )
  fun_mold <- add_bind(
    quote(vec_rep(list(!!name_sym), times = `biocmask:::ctx:::n`)),
    .env_expr = env_asis,
    .env_bind = env_pronoun,
    type = "active"
  )
  purrr::walk(row_names, fun_asis) |>
    purrr::walk(fun_mold)
  # whenever a new row binding is made,
  # execute the prior functions
  mask_rows$on_bind(fun_asis)$on_bind(fun_mold)
  # allow row data to be seen from colData context
  mask_cols$environments@env_foreign_data |>
    env_bind(
      .rows_asis = as_data_pronoun(env_asis),
      .rows = as_data_pronoun(env_pronoun)
    )
  invisible(env_pronoun)
}

connect_cols_to_rows <- function(mask_rows, mask_cols) {
  col_names <- mask_cols$names
  size <- length(col_names) + 20L
  env_mask_bind <- mask_rows$environments[[1]] #last level
  env_asis <- new.env(hash = TRUE, parent = env_mask_bind, size = size)
  env_pronoun <- new.env(hash = TRUE, parent = env_asis, size = size)
  # bind
  fun_asis <- add_bind(
    # col data may be grouped. use vctrs::vec_c to concatenate vectors
    quote(vec_c(splice(.subset(!!name_sym, `biocmask:::cols:::current_chops`)))),
    .env_expr = mask_cols$environments@env_data_chop,
    .env_bind = env_asis,
    type = "active"
  )
  fun_mold <- add_bind(
    quote(vec_rep(list(!!name_sym), times = `biocmask:::ctx:::n`)),
    .env_expr = env_asis,
    .env_bind = env_pronoun,
    type = "active"
  )
  purrr::walk(col_names, fun_asis) |>
    purrr::walk(fun_mold)
  # whenever a new col binding is made,
  # execute the prior functions
  mask_cols$on_bind(fun_asis)$on_bind(fun_mold)
  # allow col data to be seen from rowData context
  mask_rows$environments@env_foreign_data |>
    env_bind(
      .cols_asis = as_data_pronoun(env_asis),
      .cols = as_data_pronoun(env_pronoun)
    )
  invisible(env_pronoun)
}

connect_masks <- function(mask_assays, mask_rows, mask_cols) {

  col2row <- connect_cols_to_rows(mask_cols = mask_cols, mask_rows = mask_rows)
  row2col <- connect_rows_to_cols(mask_rows = mask_rows, mask_cols = mask_cols)
  col2assay <- connect_cols_to_assays(mask_cols = mask_cols, mask_assays = mask_assays)
  row2assay <- connect_rows_to_assays(mask_rows = mask_rows, mask_assays = mask_assays)
  assay2row <- connect_assays_to_rows(mask_assays = mask_assays, mask_rows = mask_rows)
  assay2col <- connect_assays_to_cols(mask_assays = mask_assays, mask_cols = mask_cols)
  list(
    assays = list(
      cols = col2assay,
      rows = row2assay
    ),
    rows = list(
      assays = assay2row,
      cols = col2row
    ),
    cols = list(
      assays = assay2col,
      rows = row2col
    )
  )

}


wrap <- function(obj) UseMethod("wrap")
wrap.vctrs_grouped_list <- function(obj) obj
wrap.default <- function(obj) list(obj)

# library(bench)
# box::use(vctrs[vec_rep_each, vec_rep])
# recycle <- function() {
#   mat * vec
# }
# force_rep <- function() {
#    mat * rep(vec, each = nrow(mat))
# }
# force_rep_vctrs <- function() {
#   mat * vec_rep_each(vec, times = nrow(mat))
# }
# double_trans <- function() {
#   t( vec * t(mat) )
# }
# vec <- rnorm(1e4)
# mat <- matrix(rnorm(1e7), ncol=1e4)
# # all.equal(force_rep(), double_trans())
# mark(double_trans(), force_rep(), force_rep_vctrs())
#
