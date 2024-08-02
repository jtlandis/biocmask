
#' @title New Biocmask
#' @name biocmask
#' @description
#' Create a biocmask for an object
#' @param obj Dispatch Object
#' @param ... Not used
#' @export
new_biocmask <- function(obj, ...) {
  UseMethod("new_biocmask")
}

#' @rdname biocmask
#' @export
new_biocmask.SummarizedExperiment <- function(obj, ...) {
  # browser()
  groups <- group_details(obj)
  expanded <- expand_groups2(groups$row_groups, groups$col_groups)
  shared_ctx_env <- prepare_shared_ctx_env(groups, expanded)
  mask_assay <- biocmask_assay$new(assays(obj),
                                   get_group_indices(groups, expanded, "assay"),
                                   .env = shared_ctx_env,
                                   .nrow = nrow(obj),
                                   .ncol = ncol(obj))
  mask_rows <- biocmask$new(rowData(obj),
                            get_group_indices(groups, expanded, "rowData"),
                            .env = shared_ctx_env)
  mask_cols <- biocmask$new(colData(obj),
                            get_group_indices(groups, expanded, "colData"),
                            .env = shared_ctx_env)
  
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

biocmask_evaluate <- function(mask, quos, ctxs, nams, env) {
  .call <- caller_call()
  n_quo <- length(quos)
  try_fetch(
    {
      for(i in seq_len(n_quo)) {
        quo <- quos[[i]]
        nm <- nams[i]
        mask$ctx <- ctxs[[i]]
        mask$eval(quo, name = nm, env = env)
      }
    },
    error = function(cnd) {
      current_ctx <- mask$ctx
      current_gid <- mask$group_id
      cli::cli_abort(
        message = "an error occured in group {current_gid} of `{current_ctx}` context",
        parent = cnd,
        call = .call,
        class = "biocmask_dplyr_eval_error"
      )
    }
  )
  invisible(mask)
}
