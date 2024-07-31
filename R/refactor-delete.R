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

  # lst <- list(assays = mask_assays, rows = mask_rows, cols = mask_cols)
  # env_mask_top <- lst |>
  #   lapply(`[[`, "environments") |>
  #   lapply(`[[`, 1L)
  # mask_names <- lst |>
  #   lapply(`[[`, "names") |>
  #   lapply(function(obj) lapply(obj, as.name))
  # mask_sizes <- mask_names |> lapply(function(x) length(x) + 20L)
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

biocmask_SummarizedExperiment <- function(se) {
  groups <- metadata(se)[["group_data"]]
  shared_ctx_env <- prepare_shared_ctx_env(group_details(se))
  mask_assay <- biocmask_assay$new(assays(se), get_group_indices(groups, "assay"), .env = shared_ctx_env,
                                   .nrow = nrow(se), .ncol = ncol(se))
  mask_rows <- biocmask$new(rowData(se), get_group_indices(groups, "rowData"), .env = shared_ctx_env)
  mask_cols <- biocmask$new(colData(se), get_group_indices(groups, "colData"), .env = shared_ctx_env)

  extended_environments <- connect_masks(mask_assays = mask_assay,
                                         mask_rows = mask_rows,
                                         mask_cols = mask_cols)

  biocmask_manager$new(.data = se,
                       .masks = list(assays = mask_assay,
                                     rows = mask_rows,
                                     cols = mask_cols),
                       .ctx_env = shared_ctx_env,
                       .extended_env = extended_environments)
}

library(dplyr)
summarise(iris, .groups = "keep")
biocmask_SummarizedExperiment(se) -> out

  ## size bindings
  # grouped_rows <- grouped_cols <- FALSE
  # if (!is.null(groups$row_groups))
  #   grouped_rows <- TRUE
  # if (!is.null(groups$col_groups))
  #   grouped_cols <- TRUE
  # type <- group_type(groups)
  # switch(
  #   type,
  #   rowcol = {
  #     env_bind_active(
  #       rows_envs@env_current_group_info,
  #       `biocmask:::current_group_size` = new_function(
  #         pairlist(),
  #         quote(.nrow),
  #         assay_envs@env_current_group_info
  #       )
  #     )
  #
  #     env_bind_active(
  #       cols_envs@env_current_group_info,
  #       `biocmask:::current_group_size` = new_function(
  #         pairlist(),
  #         quote(.ncol),
  #         assay_envs@env_current_group_info
  #       )
  #     )
  #
  #   },
  #   row = {
  #
  #     env_bind_active(
  #       rows_envs@env_current_group_info,
  #       `biocmask:::current_group_size` = new_function(
  #         pairlist(),
  #         quote(.nrow),
  #         assay_envs@env_current_group_info
  #       )
  #     )
  #     env_bind(
  #       cols_envs@env_current_group_info,
  #       `biocmask:::current_group_size` = assay_envs@env_current_group_info$.ncol
  #     )
  #   },
  #   col = {
  #     env_bind_active(
  #       cols_envs@env_current_group_info,
  #       `biocmask:::current_group_size` = new_function(
  #         pairlist(),
  #         quote(.ncol),
  #         assay_envs@env_current_group_info
  #       )
  #     )
  #     env_bind(
  #       rows_envs@env_current_group_info,
  #       `biocmask:::current_group_size` = assay_envs@env_current_group_info$.nrow
  #     )
  #   }
  # )
  #
  # # pronouns
  # ## assay binds pronouns
  # env_bind(
  #   assay_envs@env_foreign_data,
  #   .assays = as_data_pronoun(assay_envs@env_data_lazy),
  #   .rows = as_data_pronoun(rows_envs@env_data_lazy),
  #   .cols = as_data_pronoun(cols_envs@env_data_lazy)
  # )
  # ## pronouns for row env
  # env_bind(
  #   rows_envs@env_foreign_data,
  #   .assays = as_data_pronoun(assay_envs@env_row_ctx),
  #   .rows = as_data_pronoun(rows_envs@env_data_lazy),
  #   .cols = as_data_pronoun(cols_envs@env_data_lazy)
  # )
  # ## pronouns for col env
  # env_bind(
  #   cols_envs@env_foreign_data,
  #   .assays = as_data_pronoun(assay_envs@env_col_ctx),
  #   .rows = as_data_pronoun(rows_envs@env_data_lazy),
  #   .cols = as_data_pronoun(cols_envs@env_data_lazy)
  # )
  #
  # env_bind_active(
  #   assay_envs@env_current_group_info,
  #   `biocmask:::row_group_id` = new_function(pairlist(), quote(`biocmask:::current_group_id`), rows_envs@env_current_group_info),
  #   `biocmask:::col_group_id` = new_function(pairlist(), quote(`biocmask:::current_group_id`), cols_envs@env_current_group_info)
  # )
  #
  # # New Bindings
  # ## Assays
  # ### bindings for row data
  # bind_assays_in_rowdata <- add_bind(
  #   quote(vec_chop_assays_row(.assays[[!!name]], seq_len(`biocmask:::current_group_size`))),
  #   .env_expr = rows_envs@env_foreign_data,
  #   type = "active"
  # )
  # lapply(mask_assay$names, bind_assays_in_rowdata)
  # mask_assay$on_bind(bind_assays_in_rowdata)
  # ### bindings for col data
  # bind_assays_in_coldata <- add_bind(
  #   quote(vec_chop_assays_col(.assays[[!!name]], seq_len(`biocmask:::current_group_size`))),
  #   .env_expr = cols_envs@env_foreign_data,
  #   type = "active"
  # )
  # lapply(mask_assay$names, bind_assays_in_coldata)
  # mask_assay$on_bind(bind_assays_in_coldata)
  #
  # ## rows
  # ### binds for col data
  # bind_rows_in_coldata <- add_bind(
  #   quote(vec_rep_each(wrap(.rows[[!!name]]), times = `biocmask:::current_group_size`)),
  #   .env_expr = cols_envs@env_foreign_data,
  #   type = "active"
  # )
  # lapply(mask_rows$names, bind_rows_in_coldata)
  # mask_rows$on_bind(bind_rows_in_coldata)
  # ### binds for assays
  # bind_rows_in_assays <- add_bind(
  #   quote(vec_rep(.rows[[!!name]], times = .ncol)),
  #   .env_expr = assay_envs@env_foreign_data,
  #   type = "active"
  # )
  # lapply(mask_rows$names, bind_rows_in_assays)
  # mask_rows$on_bind(bind_rows_in_assays)
  #
  # ## cols
  # ### binds for row data
  # bind_cols_in_rowdata <- add_bind(
  #   quote(vec_rep_each(wrap(.cols[[!!name]]), times = `biocmask:::current_group_size`)),
  #   .env_expr = rows_envs@env_foreign_data,
  #   type = "active"
  # )
  # lapply(mask_cols$names, bind_cols_in_rowdata)
  # mask_cols$on_bind(bind_cols_in_rowdata)
  # ### binds for assays
  # bind_cols_in_assays <- add_bind(
  #   quote(vec_rep_each(.cols[[!!name]], times = .nrow)),
  #   .env_expr = assay_envs@env_foreign_data,
  #   type = "active"
  # )
  # lapply(mask_cols$names, bind_cols_in_assays)
  # mask_cols$on_bind(bind_cols_in_assays)
  #
  #
  #
  # mask_assay$on_bind(
  #   add_bind(
  #     quote(lapply(1:.nrow, function(i, x) x[i,,drop=TRUE], x = .assays[[!!name]])),
  #     .env_expr = mask_rows$environments[[3]]
  #     .env_bind = mask_assay$environments[[4]],
  #     type = "lazy"
  # )
  #   )

# }

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
