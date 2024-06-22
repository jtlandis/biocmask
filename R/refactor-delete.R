

biocmask_SummarizedExperiment <- function(se) {
  groups <- metadata(se)[["group_data"]]
  mask_assay <- biocmask_assay$new(assays(se), get_group_indices(groups, "assay"),
                                   .nrow = nrow(se), .ncol = ncol(se))
  mask_rows <- biocmask$new(rowData(se), get_group_indices(groups, "rowData"))
  mask_cols <- biocmask$new(colData(se), get_group_indices(groups, "colData"))
  assay_envs <- mask_assay$environments
  rows_envs <- mask_rows$environments
  cols_envs <- mask_cols$environments

  ## size bindings
  grouped_rows <- grouped_cols <- FALSE
  if (!is.null(groups$row_groups))
    grouped_rows <- TRUE
  if (!is.null(groups$col_groups))
    grouped_cols <- TRUE
  type <- group_type(groups)
  switch(
    type,
    rowcol = {
      env_bind_active(
        rows_envs@env_current_group_info,
        `biocmask:::current_group_size` = new_function(
          pairlist(),
          quote(.nrow),
          assay_envs@env_current_group_info
        )
      )

      env_bind_active(
        cols_envs@env_current_group_info,
        `biocmask:::current_group_size` = new_function(
          pairlist(),
          quote(.ncol),
          assay_envs@env_current_group_info
        )
      )

    },
    row = {

      env_bind_active(
        rows_envs@env_current_group_info,
        `biocmask:::current_group_size` = new_function(
          pairlist(),
          quote(.nrow),
          assay_envs@env_current_group_info
        )
      )
      env_bind(
        cols_envs@env_current_group_info,
        `biocmask:::current_group_size` = assay_envs@env_current_group_info$.ncol
      )
    },
    col = {
      env_bind_active(
        cols_envs@env_current_group_info,
        `biocmask:::current_group_size` = new_function(
          pairlist(),
          quote(.ncol),
          assay_envs@env_current_group_info
        )
      )
      env_bind(
        rows_envs@env_current_group_info,
        `biocmask:::current_group_size` = assay_envs@env_current_group_info$.nrow
      )
    }
  )

  # pronouns
  ## assay binds pronouns
  env_bind(
    assay_envs@env_foreign_data,
    .assays = as_data_pronoun(assay_envs@env_data_lazy),
    .rows = as_data_pronoun(rows_envs@env_data_lazy),
    .cols = as_data_pronoun(cols_envs@env_data_lazy)
  )
  ## pronouns for row env
  env_bind(
    rows_envs@env_foreign_data,
    .assays = as_data_pronoun(assay_envs@env_row_ctx),
    .rows = as_data_pronoun(rows_envs@env_data_lazy),
    .cols = as_data_pronoun(cols_envs@env_data_lazy)
  )
  ## pronouns for col env
  env_bind(
    cols_envs@env_foreign_data,
    .assays = as_data_pronoun(assay_envs@env_col_ctx),
    .rows = as_data_pronoun(rows_envs@env_data_lazy),
    .cols = as_data_pronoun(cols_envs@env_data_lazy)
  )

  env_bind_active(
    assay_envs@env_current_group_info,
    `biocmask:::row_group_id` = new_function(pairlist(), quote(`biocmask:::current_group_id`), rows_envs@env_current_group_info),
    `biocmask:::col_group_id` = new_function(pairlist(), quote(`biocmask:::current_group_id`), cols_envs@env_current_group_info)
  )

  # New Bindings
  ## Assays
  ### bindings for row data
  bind_assays_in_rowdata <- add_bind(
    quote(vec_chop_assays_row(.assays[[!!name]], seq_len(`biocmask:::current_group_size`))),
    .env_expr = rows_envs@env_foreign_data,
    type = "active"
  )
  lapply(mask_assay$names, bind_assays_in_rowdata)
  mask_assay$on_bind(bind_assays_in_rowdata)
  ### bindings for col data
  bind_assays_in_coldata <- add_bind(
    quote(vec_chop_assays_col(.assays[[!!name]], seq_len(`biocmask:::current_group_size`))),
    .env_expr = cols_envs@env_foreign_data,
    type = "active"
  )
  lapply(mask_assay$names, bind_assays_in_coldata)
  mask_assay$on_bind(bind_assays_in_coldata)

  ## rows
  ### binds for col data
  bind_rows_in_coldata <- add_bind(
    quote(vec_rep_each(wrap(.rows[[!!name]]), times = `biocmask:::current_group_size`)),
    .env_expr = cols_envs@env_foreign_data,
    type = "active"
  )
  lapply(mask_rows$names, bind_rows_in_coldata)
  mask_rows$on_bind(bind_rows_in_coldata)
  ### binds for assays
  bind_rows_in_assays <- add_bind(
    quote(vec_rep(.rows[[!!name]], times = .ncol)),
    .env_expr = assay_envs@env_foreign_data,
    type = "active"
  )
  lapply(mask_rows$names, bind_rows_in_assays)
  mask_rows$on_bind(bind_rows_in_assays)

  ## cols
  ### binds for row data
  bind_cols_in_rowdata <- add_bind(
    quote(vec_rep_each(wrap(.cols[[!!name]]), times = `biocmask:::current_group_size`)),
    .env_expr = rows_envs@env_foreign_data,
    type = "active"
  )
  lapply(mask_cols$names, bind_cols_in_rowdata)
  mask_cols$on_bind(bind_cols_in_rowdata)
  ### binds for assays
  bind_cols_in_assays <- add_bind(
    quote(vec_rep_each(.cols[[!!name]], times = .nrow)),
    .env_expr = assay_envs@env_foreign_data,
    type = "active"
  )
  lapply(mask_cols$names, bind_cols_in_assays)
  mask_cols$on_bind(bind_cols_in_assays)



  mask_assay$on_bind(
    add_bind(
      quote(lapply(1:.nrow, function(i, x) x[i,,drop=TRUE], x = .assays[[!!name]])),
      .env_expr = mask_rows$environments[[3]]
      .env_bind = mask_assay$environments[[4]],
      type = "lazy"
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
