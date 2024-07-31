
#' @name biocmask
#' @description
#' Create a biocmask for an object
#' @param obj Dispatch Object
#' @param ... Not used
new_biocmask <- function(obj, ...) {
  UseMethod("biocmask", obj)
}


new_biocmask.SummarizedExperiment <- function(obj, ...) {
  groups <- metadata(obj)[["group_data"]]
  shared_ctx_env <- prepare_shared_ctx_env(group_details(obj))
  mask_assay <- biocmask_assay$new(assays(obj), get_group_indices(groups, "assay"), .env = shared_ctx_env,
                                   .nrow = nrow(obj), .ncol = ncol(obj))
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