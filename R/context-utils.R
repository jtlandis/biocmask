
#' @title biocmask contexts
#' @name biocmask-context
#' @description
#' Contextual user-facing helper function for dplyr verbs with SummarizedExperiment
#' objects. These functions are intended to be used as the top level call to
#' any dplyr verbs `...` argument, similar to that of `across()`/`if_any()`/`if_all()`.
#' 
#' @param ... expressions to evaluate within its associated context
NULL

#' @rdname biocmask-context 
#' @description
#' Specifies that the following expressions should be evaluated within the 
#' colData context.
#' @export
cols <- function(...) {
  abort("`cols()` is a sentinal function for SummarizedExperiment dplyr verbs")
}

#' @rdname biocmask-context 
#' @description
#' Specifies that the following expressions should be evaluated within the 
#' rowData context.
#' @export
rows <- function(...) {
  abort("`cols()` is a sentinal function for SummarizedExperiment dplyr verbs")
}


#' @rdname biocmask-context
#' @description
#' Specify a single expression to evaluate in another context
#' 
#' @export
col_ctx <- function(x) {
  env <- peek_ctx("biocmask:::caller_env")
  biocmanager <- peek_ctx("biocmask:::manager")
  ctx <- biocmanager$ctx
  if (ctx=="cols") abort("`col_ctx()` within cols(...) is redunant")
  quo <- new_quosure(enexpr(x), env = env)
  bot_env <- biocmanager$extended[["cols"]]
  mask <- new_data_mask(bot_env, top_env)
  eval_tidy(quo, data = mask, env = env)
}

#' @rdname biocmask-context
#' @description
#' Specify a single expression to evaluate in another context
#' 
#' @export
row_ctx <- function(x) {
  env <- peek_ctx("biocmask:::caller_env")
  biocmanager <- peek_ctx("biocmask:::manager")
  ctx <- biocmanager$ctx
  if (ctx=="rows") abort("`row_ctx()` within rows(...) is redunant")
  quo <- new_quosure(enexpr(x), env = env)
  bot_env <- biocmanager$extended[["rows"]]
  mask <- new_data_mask(bot_env, top_env)
  eval_tidy(quo, data = mask, env = env)
}

#' @rdname biocmask-context
#' @description
#' Specify a single expression to evaluate in another context
#' 
#' @export
assay_ctx <- function(x) {
  env <- peek_ctx("biocmask:::caller_env")
  biocmanager <- peek_ctx("biocmask:::manager")
  ctx <- biocmanager$ctx
  if (ctx=="assays") abort("`assay_ctx()` at top level ... is redundant")
  quo <- new_quosure(enexpr(x), env = env)
  bot_env <- biocmanager$extended[["assays"]]
  mask <- new_data_mask(bot_env, top_env)
  eval_tidy(quo, data = mask, env = env)
}

# rows = function(...) {
#   # browser()
#   mask_manager <- peek_ctx("SE:::mask_manager")
#   fn <- peek_ctx("SE:::dplyr_function")
#   env <- peek_ctx("SE:::caller_env")
#   eval_fun <- switch(fn,
#                      mutate = mask_manager$eval_mutate_rows,
#                      group_by = mask_manager$eval_mutate_rows,
#                      stop(sprintf("`%s` is not yet implemented", fn)))
#   quos <- enquos(..., .named = TRUE)
#   nms <- names(quos)
#   for (i in seq_along(quos)) {
#     quo <- rlang::quo_set_env(quos[[i]], env)
#     name <- nms[i]
#     eval_fun(quo, name)
#   }
#   skip()
# },
# cols = function(...) {
#   mask_manager <- peek_ctx("SE:::mask_manager")
#   fn <- peek_ctx("SE:::dplyr_function")
#   env <- peek_ctx("SE:::caller_env")
#   eval_fun <- switch(fn,
#                      mutate = mask_manager$eval_mutate_cols,
#                      group_by = mask_manager$eval_mutate_cols,
#                      stop(sprintf("`%s` is not yet implemented", fn)))
#   quos <- enquos(..., .named = TRUE)
#   nms <- names(quos)
#   for (i in seq_along(quos)) {
#     quo <- rlang::quo_set_env(quos[[i]], env)
#     name <- nms[i]
#     eval_fun(quo, name)
#   }
#   skip()
# }