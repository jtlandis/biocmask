

biocmask_SummarizedExperiment <- function(se) {
  groups <- metadata(se)[["group_data"]]
  mask_assay <- biocmask_assay$new(assays(se), get_group_indices(groups, "assay"),
                                   .nrow = nrow(se), .ncol = ncol(se))
  mask_rows <- biocmask$new(rowData(se), get_group_indices(groups, "rowData"))
  mask_cols <- biocmask$new(colData(se), get_group_indices(groups, "colData"))
  assay_envs <- mask_assay$environments
  rows_envs <- mask_rows$environments
  cols_envs <- mask_cols$environments
  env_bind(
    assay_envs[[4]],
    .assays = as_data_pronoun(assay_envs[[3]])
    .rows = as_data_pronoun(rows_envs[[3]])
    .cols = as_data_pronoun(cols_envs[[3]])
  )
  env_bind(
    assay_envs[[4]],
    .assays = as_data_pronoun(assay_envs[[3]])
    .rows = as_data_pronoun(rows_envs[[3]])
    .cols = as_data_pronoun(cols_envs[[3]])
  )
  env_bind(
    assay_envs[[4]],
    .assays = as_data_pronoun(assay_envs[[3]])
    .rows = as_data_pronoun(rows_envs[[3]])
    .cols = as_data_pronoun(cols_envs[[3]])
  )
  ## b
  mask_assay$on_bind(
    add_bind(
      quote(lapply(1:.nrow, function(i, x) x[i,,drop=TRUE], x = .assays[[!!name]])), 
      .env_expr = mask_rows$environments[[3]]
      .env_bind = mask_assay$environments[[4]],
      type = "lazy"
  )
    )
  
}

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
