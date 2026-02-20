#' @title New Biocmask
#' @name new_biocmask_manager
#' @description
#' Create a biocmask for an object
#' @param obj Dispatch Object
#' @param ... Not used
#' @return a biocmask_manager R6 class object
#' @seealso [biocmask::BiocmaskManager]
#' @examples
#'
#' manager <- new_biocmask_manager(se_simple)
#' manager$ctx
#' q <- biocmask_quos(
#'   counts_1 = counts + 1,
#'   cols(is_drug = condition == "drug"),
#'   .ctx_default = "assays",
#'   .ctx_opt = c("rows", "cols")
#' )
#' manager$eval(q[[1]])
#' manager$results()
#' # evaluating second quo without switching contexts will error
#' manager$eval(q[[2]]) |> try()
#' manager$ctx <- "cols"
#' manager$ctx
#' manager$eval(q[[2]])
#' manager$results()
#'
#' @export
new_biocmask_manager <- function(obj, ...) {
  UseMethod("new_biocmask_manager")
}

#' @rdname new_biocmask_manager
#' @export
new_biocmask_manager.SummarizedExperiment <- function(obj, ...) {
  groups <- group_details(obj)
  expanded <- expand_groups2(groups$row_groups, groups$col_groups)
  nr <- nrow(obj)
  nc <- ncol(obj)
  shared_ctx_env <- prepare_shared_ctx_env(groups = groups, expanded = expanded)

  mask_assay <- biocmask_assay$new(
    assays(obj),
    get_group_indices(groups, expanded, "assay"),
    .nrow = nr,
    .ncol = nc,
    .env_bot = shared_ctx_env,
    .env_top = top_env
  )
  mask_rows <- biocmask$new(
    prepend_rownames(rowData(obj), column = ".features"),
    get_group_indices(groups, expanded, "rowData"),
    .env_bot = shared_ctx_env,
    .env_top = top_env
  )
  mask_cols <- biocmask$new(
    prepend_rownames(colData(obj), column = ".samples"),
    get_group_indices(groups, expanded, "colData"),
    .env_bot = shared_ctx_env,
    .env_top = top_env
  )

  extended_environments <- connect_masks(
    mask_assays = mask_assay,
    mask_rows = mask_rows,
    mask_cols = mask_cols
  )

  biocmask_manager$new(
    .data = obj,
    .masks = list(assays = mask_assay, rows = mask_rows, cols = mask_cols),
    .ctx_env = shared_ctx_env,
    .extended_env = extended_environments
  )
}

get_biocmask_quo_ctx <- function(quo, index, env = rlang::caller_env()) {
  ctx <- attr(quo, which = "biocmask:::ctx", exact = TRUE)
  if (is.null(ctx)) {
    cli::cli_abort(
      c("quosure is missing 'biocmask:::ctx' attribute.",
        i = "error occured on index {i}",
        i = "Was the quosure created without using `biocmask::biocmask_quos()`?"
      ),
      problem_quo = quo,
      call = env,
      internal = TRUE
    )
  }
  ctx
}

#' helpful wrapper to evaluate quosures in a biocmask_manager object
#' @param mask_manager A biocmask_manager object
#' @param quos A list of quosures to evaluate
#' @param ctxs A character vector of contexts for each quosure
#' @param env The environment in which to evaluate the quosures
#' @export
biocmask_manager_evaluate <- function(
  mask_manager,
  quos,
  # nams,
  env
) {
  .call <- caller_call()
  n_quo <- length(quos)
  ctxs <- map2(
    quos,
    seq_along(quos),
    get_biocmask_quo_ctx,
    env = env
  )
  try_fetch(
    {
      for (i in seq_len(n_quo)) {
        quo <- quos[[i]]
        # nm <- nams[i]
        mask_manager$ctx <- ctxs[[i]]
        mask_manager$eval(quo, env = env)
      }
    },
    error = function(cnd) {
      current_ctx <- mask_manager$ctx
      current_gid <- mask_manager$group_id
      cli::cli_abort(
        message = "an error occured in group {current_gid} of `{current_ctx}` context",
        parent = cnd,
        call = .call,
        class = "biocmask_eval_error"
      )
    }
  )
  invisible(mask_manager)
}

#' Get a biocmask R6 object
#'
#' Method to construct a biocmask R6 object. It is the caller's
#' responsibility to ensure the proper contexts are stored within
#' the environment heirarchy.
#'
#' @param .data the data to be masked
#' @param .indices a list of integer vectors for each group
#' @param .top the top environment for this mask
#' @param .bot the bottom environment for this mask
#'
#' @return some R6 object inheriting from biocmask
#' @export
new_biocmask <- function(.data, .indices, .top, .bot, ...) {
  UseMethod("new_biocmask")
}


#' @export
new_biocmask.default <- function(.data, .indices, .top, .bot, ...) {
  biocmask$new(
    .data = .data, .indices = .indices, .env_bot = .bot, .env_top = .top
  )
}
