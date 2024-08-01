
#' @name biocmask
#' @description
#' Create a biocmask for an object
#' @param obj Dispatch Object
#' @param ... Not used
new_biocmask <- function(obj, ...) {
  UseMethod("new_biocmask")
}


new_biocmask.SummarizedExperiment <- function(obj, ...) {
  groups <- metadata(obj)[["group_data"]]
  expanded_groups <- group_details(obj)
  shared_ctx_env <- prepare_shared_ctx_env(expanded_groups)
  mask_assay <- biocmask_assay$new(assays(obj), get_group_indices(groups, "assay"), .env = shared_ctx_env,
                                   .nrow = nrow(obj), .ncol = ncol(obj))
  mask_rows <- biocmask$new(rowData(obj), get_group_indices(groups, "rowData"), .env = shared_ctx_env)
  mask_cols <- biocmask$new(colData(obj), get_group_indices(groups, "colData"), .env = shared_ctx_env)
  
  extended_environments <- connect_masks(mask_assays = mask_assay,
                                         mask_rows = mask_rows,
                                         mask_cols = mask_cols)
  
  biocmask_manager$new(.data = obj,
                       .masks = list(assays = mask_assay,
                                     rows = mask_rows,
                                     cols = mask_cols),
                       .ctx_env = shared_ctx_env,
                       .extended_env = extended_environments)
}
