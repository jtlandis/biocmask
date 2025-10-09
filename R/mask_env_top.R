# `skip!` <- structure(list(), class = "skip")
# skip <- function() {
#   `skip!`
# }
# print.skip <- function(x, ...) cat("<skip>\n")
# is_skip <- function(x) inherits(x, "skip")

ctx_env <- new.env(parent = emptyenv())

peek_ctx <- function(name) {
  ctx_env[[name]]
}

poke_ctx <- function(name, value) {
  old <- ctx_env[[name]]
  ctx_env[[name]] <- value
  invisible(old)
}

poke_ctx_local <- function(name, value) {
  old <- ctx_env[[name]]
  ctx_env[[name]] <- value
  quo <- expr(ctx_env[[!!name]] <- !!old)
  do.call(
    on.exit,
    list(quo, add = TRUE),
    envir = parent.frame()
  )
  invisible(old)
}

pmap <- function(.l, .f, ...) {
  do.call(
    mapply,
    c(list(FUN = .f, MoreArgs = list2(...), SIMPLIFY = FALSE), .l)
  )
}

map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, MoreArgs = list2(...), SIMPLIFY = FALSE)
}

map <- function(.x, .f, ...) {
  lapply(X = .x, FUN = .f, ...)
}

# this is the top of all our rlang data masks (inherited from base).
# it contains all expected functions for transforming values  in multi-tiered
# data masks.
# If future extensions need access to additional functions, they should consider
# creating their own top_env, or open a PR to add it here.

#' @title Create a new top-level environment for data masking.
#' @name new_bioc_top_env
#' @description
#' Creates a new environment that inherits from the specified parent
#' environment. This environment is automatically installed with various
#' `bioc_*` functions that may be useful for the user.
#'
#' Ideally, this environment should be created once within a package's
#' namespace, and then biocmasks should construct new environments with
#' this top level as the parent environment.
#'
#' @param ... Additional arguments to pass to `new_environment()`.
#' @param parent The parent environment to inherit from.
#' @export
new_bioc_top_env <- function(..., parent = empty_env()) {
  new_environment(
    data = rlang::dots_list(
      .subset2 = base::.subset2,
      .subset = base::.subset,
      # --- symbols
      `[` = base::`[`,
      `[[` = base::`[[`,
      `:` = base::`:`,
      `::` = base::`::`,
      `$` = base::`$`,
      `@` = base::`@`,
      `{` = base::`{`,
      `(` = base::`(`,
      `!` = base::`!`,
      `~` = base::`~`,
      `^` = base::`^`,
      `&` = base::`&`,
      `|` = base::`|`,
      `+` = base::`+`,
      `-` = base::`-`,
      `*` = base::`*`,
      `/` = base::`/`,
      `<` = base::`<`,
      `>` = base::`>`,
      `<=` = base::`<=`,
      `>=` = base::`>=`,
      `=` = base::`=`,
      `<-` = base::`<-`,
      `function` = base::`function`,
      match = base::match,
      unique = base::unique,
      lapply = base::lapply,
      map = map,
      map2 = map2,
      pmap = pmap,
      seq_len = base::seq_len,
      seq_along = base::seq_along,
      list = rlang::list2,
      length = base::length,
      bioc_slice = bioc_slice,
      bioc_unchop = bioc_unchop,
      bioc_chop = bioc_chop,
      bioc_rep = bioc_rep,
      bioc_rep_each = bioc_rep_each,
      bioc_c = bioc_c,
      splice = splice,
      # debating if the below is necessary. Currently not used.
      poke_ctx = poke_ctx,
      poke_ctx_local = poke_ctx_local,
      peek_ctx = peek_ctx,
      .current_group_id = 0L,
      .mask_manager = NULL,
      ...,
      .homonyms = "last"
    ),
    parent = parent
  )
}

#' @title Create new environment prior to biocmask.
#' @description dictate the default context as well as the group id.
#' @param context The context to use for the new environment.
#' @param parent The parent environment to inherit from. Ideally this is the
#' environment created by `new_bioc_top_env()`.
#' @export
new_bioc_bot_env <- function(..., context = NULL, parent = top_env) {
  if (!is.null(context)) {
    stopifnot(is.character(context), length(context) == 1)
  }
  new_environment(
    data = rlang::dots_list(
      # current context
      `biocmask:::ctx` = context,
      # context group id
      `biocmask:::ctx:::group_id` = 1L,
      ...,
      .homonyms = "last"
    ),
    parent = parent
  )
}

# top_env <- new_environment(
#   data = list(
#     vec_chop = vctrs::vec_chop,
#     vec_chop_assays = vec_chop_assays,
#     vec_chop_assays_row = vec_chop_assays_row,
#     vec_chop_assays_col = vec_chop_assays_col,
#     bioc_rep = bioc_rep,
#     bioc_rep_each = bioc_rep_each,
#     vec_c = vctrs::vec_c,
#     splice = splice,
#     # skip = skip,
#     poke_ctx = poke_ctx,
#     poke_ctx_local = poke_ctx_local,
#     peek_ctx = peek_ctx,
#     .current_group_id = 0L,
#     .mask_manager = NULL
#   ),
#   parent = baseenv()
# )

# bot_env <- new.env(parent = top_env)

# biocmask_group_ids2 <- function(
#   groups,
#   expanded,
#   relative_to = c("assays", "rows", "cols")
# ) {
#   # browser()
#   relative_to <- match.arg(relative_to, c("assays", "rows", "cols"))
#   Nr <- nrow(groups$row_groups)
#   Nc <- nrow(groups$col_groups)
#   switch(
#     relative_to,
#     assays = {
#       out <- expanded
#       list(
#         assays = as.list(out[[".group_id"]]),
#         rows = as.list(out[[".rows::.indices_group_id"]]),
#         cols = as.list(out[[".cols::.indices_group_id"]]),
#         .nrow = as.list(out[[".nrows"]]),
#         .ncol = as.list(out[[".ncols"]]),
#         .nsize = as.list(out[[".nrows"]] * out[[".ncols"]])
#       )
#     },
#     rows = {
#       g_row <- groups$row_groups
#       g_ind <- as.list(g_row[[".indices_group_id"]])
#       c_ind <- groups$col_groups[[".indices_group_id"]]
#       r_nrow <- vapply(g_row[[".indices"]], length, 1L)
#       r_ncol <- vapply(groups$col_groups[[".indices"]], length, 1L)
#       r_ncol <- bioc_rep(sum(r_ncol), Nr)
#       r_nsiz <- r_nrow * r_ncol
#       list(
#         assays = lapply(g_ind, function(i) Nr * (c_ind - 1) + i),
#         rows = g_ind,
#         cols = bioc_rep(list(c_ind), Nr),
#         .nrow = as.list(r_nrow),
#         .ncol = as.list(r_ncol),
#         .nsize = as.list(r_nsiz)
#       )
#     },
#     cols = {
#       g_col <- groups$col_groups
#       g_ind <- as.list(g_col[[".indices_group_id"]])
#       r_ind <- groups$row_groups[[".indices_group_id"]]
#       c_ncol <- vapply(g_col[[".indices"]], length, 1L)
#       c_nrow <- vapply(groups$row_groups[[".indices"]], length, 1L)
#       c_nrow <- bioc_rep(sum(c_nrow), Nc) #?
#       c_nsiz <- c_nrow * c_ncol
#       list(
#         assays = lapply(g_ind, function(i) Nr * (i - 1L) + r_ind),
#         rows = bioc_rep(list(r_ind), Nc),
#         cols = g_ind,
#         .nrow = as.list(c_nrow),
#         .ncol = as.list(c_ncol),
#         .nsize = as.list(c_nsiz)
#       )
#     }
#   )
# }

# biocmask_group_ids <- function(.data, var) {
#   group_by(.data, {{ var }}) |>
#     mutate(rows_keep = !duplicated(.rows_group_id),
#            cols_keep = !duplicated(.cols_group_id)) |>
#     summarise(assays = list(.group_id[rows_keep | cols_keep]),
#               rows = list(.rows_group_id[rows_keep]),
#               cols = list(.cols_group_id[cols_keep]),
#               .nrow = list(sum(.nrows[rows_keep])),
#               .ncol = list(sum(.ncols[cols_keep])),
#               .nsize = list(.nrow[[1]]*.ncol[[1]])) |>
#     select(- {{ var }}) |>
#     as.list()
# }

# env_group_id <- function(env) {
#   force(env)
#   function(id) {
#     env[["biocmask:::ctx:::group_id"]] <- id
#     invisible(id)
#   }
# }

# # provides most of the connectivity for tracking how other
# # contexts should collect certain data. Most of this headache is to support
# # grouping operations.
# prepare_shared_ctx_env <- function(groups, expanded) {
#   ind_d <- attr(groups, "obj_dim")
#
#   inf_assay <- biocmask_group_ids2(groups, expanded, "assays")
#   inf_rows <- biocmask_group_ids2(groups, expanded, "rows")
#   inf_cols <- biocmask_group_ids2(groups, expanded, "cols")
#
#   assay_group_id = list(
#     assays = inf_assay[["assays"]],
#     rows = inf_rows[["assays"]],
#     cols = inf_cols[["assays"]]
#   )
#   rows_group_id = list(
#     assays = inf_assay[["rows"]],
#     rows = inf_rows[["rows"]],
#     cols = inf_cols[["rows"]]
#   )
#   cols_group_id = list(
#     assays = inf_assay[["cols"]],
#     rows = inf_rows[["cols"]],
#     cols = inf_cols[["cols"]]
#   )
#   ctx_group_id = list(
#     assays = inf_assay[["assays"]],
#     rows = inf_rows[["rows"]],
#     cols = inf_cols[["cols"]]
#   )
#
#   nrow_info = list(
#     assays = inf_assay[[".nrow"]],
#     rows = inf_rows[[".nrow"]],
#     cols = inf_cols[[".nrow"]]
#   )
#   ncol_info = list(
#     assays = inf_assay[[".ncol"]],
#     rows = inf_rows[[".ncol"]],
#     cols = inf_cols[[".ncol"]]
#   )
#   nsize_info = list(
#     assays = inf_assay[[".nsize"]],
#     rows = inf_rows[[".nsize"]],
#     cols = inf_cols[[".nsize"]]
#   )
#   # this may be incorrect???
#   nsize_ctx = list(
#     assays = inf_assay[[".nsize"]],
#     rows = inf_rows[[".nrow"]],
#     # is .ncol refering to size of output?
#     cols = inf_cols[[".ncol"]]
#   )
#
#   shared_ctx_env <- new_environment(
#     data = list(
#       #current context
#       `biocmask:::ctx` = "assays",
#       #context group id
#       `biocmask:::ctx:::group_id` = 1L,
#       # list of all contexts, each contains
#       # indices to retrieve data from that context
#       # while evaluating from assays context
#       `biocmask:::assays:::group_chop_ids` = assay_group_id,
#       # list of all contexts, each contains
#       # indices to retrieve data from that context
#       # while evaluating from rows context
#       `biocmask:::rows:::group_chop_ids` = rows_group_id,
#       # list of all contexts, each contains
#       # indices to retrieve data from that context
#       # while evaluating from cols context
#       `biocmask:::cols:::group_chop_ids` = cols_group_id,
#       # shortcut to the current context's indices
#       `biocmask:::ctx:::group_chop_ids` = ctx_group_id,
#       # list of all contexts, each contains
#       # the number of rows for the context
#       `biocmask:::dim:::nrow` = nrow_info,
#       # list of all contexts, each contains
#       # the number of cols for the context
#       # NOTE: assays context reports its dims, NOT how many assays
#       `biocmask:::dim:::ncol` = ncol_info,
#       # list of all contexts, each contains
#       # expected size of output... example:
#       # assays -> nrow * ncol
#       # rows -> nrow of rowData
#       # cols -> nrow of colData
#       `biocmask:::dim:::size` = nsize_info,
#       `biocmask:::dim:::n` = nsize_ctx,
#       `biocmask:::ctx:::n_groups` = expanded |>
#         summarise(
#           assays = max(.group_id),
#           rows = max(`.rows::.indices_group_id`),
#           cols = max(`.cols::.indices_group_id`)
#         ) |>
#         as.list()
#     ),
#     parent = bot_env
#   )
#
#   # set of active bindings to retrieve above info dynamically
#   env_bind_active(
#     shared_ctx_env,
#     # number of groups for current context
#     `biocmask:::n_groups` = new_function(
#       pairlist(),
#       expr(.subset2(`biocmask:::ctx:::n_groups`, `biocmask:::ctx`)),
#       env = shared_ctx_env
#     ),
#     # indices of current group in current context
#     `biocmask:::ctx:::current_chops` = new_function(
#       pairlist(),
#       expr(
#         .subset2(`biocmask:::ctx:::group_chop_ids`, `biocmask:::ctx`) |>
#           .subset2(`biocmask:::ctx:::group_id`)
#       ),
#       env = shared_ctx_env
#     ),
#     # assays indices for current group
#     `biocmask:::assays:::current_chops` = new_function(
#       pairlist(),
#       expr(
#         .subset2(`biocmask:::assays:::group_chop_ids`, `biocmask:::ctx`) |>
#           .subset2(`biocmask:::ctx:::group_id`)
#       ),
#       env = shared_ctx_env
#     ),
#     # rowData indices for current group
#     `biocmask:::rows:::current_chops` = new_function(
#       pairlist(),
#       expr(
#         .subset2(`biocmask:::rows:::group_chop_ids`, `biocmask:::ctx`) |>
#           .subset2(`biocmask:::ctx:::group_id`)
#       ),
#       env = shared_ctx_env
#     ),
#     # colData indices for current group
#     `biocmask:::cols:::current_chops` = new_function(
#       pairlist(),
#       expr(
#         .subset2(`biocmask:::cols:::group_chop_ids`, `biocmask:::ctx`) |>
#           .subset2(`biocmask:::ctx:::group_id`)
#       ),
#       env = shared_ctx_env
#     ),
#     # nrow for current context and group
#     `biocmask:::ctx:::nrow` = new_function(
#       pairlist(),
#       expr(
#         .subset2(`biocmask:::dim:::nrow`, `biocmask:::ctx`) |>
#           .subset2(`biocmask:::ctx:::group_id`)
#       ),
#       env = shared_ctx_env
#     ),
#     # nrow for current context and group
#     `biocmask:::ctx:::ncol` = new_function(
#       pairlist(),
#       expr(
#         .subset2(`biocmask:::dim:::ncol`, `biocmask:::ctx`) |>
#           .subset2(`biocmask:::ctx:::group_id`)
#       ),
#       env = shared_ctx_env
#     ),
#     # size for current context and group
#     `biocmask:::ctx:::size` = new_function(
#       pairlist(),
#       expr(
#         .subset2(`biocmask:::dim:::size`, `biocmask:::ctx`) |>
#           .subset2(`biocmask:::ctx:::group_id`)
#       ),
#       env = shared_ctx_env
#     ),
#     #
#     `biocmask:::ctx:::n` = new_function(
#       pairlist(),
#       expr(
#         .subset2(`biocmask:::dim:::n`, `biocmask:::ctx`) |>
#           .subset2(`biocmask:::ctx:::group_id`)
#       ),
#       env = shared_ctx_env
#     )
#   )
#   shared_ctx_env$n <- new_function(
#     pairlist(),
#     quote(`biocmask:::ctx:::n`),
#     shared_ctx_env
#   )
#   shared_ctx_env$cur_group_id <- new_function(
#     pairlist(),
#     quote(`biocmask:::ctx:::group_id`),
#     shared_ctx_env
#   )
#   shared_ctx_env$set_group_id <- env_group_id(shared_ctx_env)
#   shared_ctx_env
# }
