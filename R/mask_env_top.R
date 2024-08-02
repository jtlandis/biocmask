
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
  quo <- rlang::expr(ctx_env[[!!name]] <- !!old)
  do.call(
    on.exit,
    list(quo, add = TRUE),
    env = parent.frame()
  )
  invisible(old)
}




top_env <- rlang::new_environment(
  data = list(
    vec_chop = vctrs::vec_chop,
    vec_chop_assays = vec_chop_assays,
    vec_chop_assays_row = vec_chop_assays_row,
    # vec_chop_assays_row.matrix = vec_chop_assays_row.matrix,
    # vec_chop_assays_row.vctrs_grouped_list = vec_chop_assays_row.vctrs_grouped_list,
    vec_chop_assays_col = vec_chop_assays_col,
    # vec_chop_assays_col.matrix = vec_chop_assays_col.matrix,
    # vec_chop_assays_col.vctrs_grouped_list = vec_chop_assays_col.vctrs_grouped_list,
    # wrap = wrap,
    # wrap.default = wrap.default,
    # wrap.vctrs_grouped_list = wrap.vctrs_grouped_list,
    # new_grouped_lst = new_grouped_lst,
    vec_rep = vctrs::vec_rep,
    vec_rep_each = vctrs::vec_rep_each,
    vec_c = vctrs::vec_c,
    # skip = skip,
    poke_ctx = poke_ctx,
    poke_ctx_local = poke_ctx_local,
    peek_ctx = peek_ctx,
    .current_group_id = 0L,
    .mask_manager = NULL
  ),
  parent = baseenv()
)

bot_env <- new.env(parent = top_env)

biocmask_group_ids2 <- function(groups, expanded, relative_to = c("assays","rows","cols")) {
  # browser()
  relative_to <- match.arg(relative_to, c("assays","rows","cols"))
  Nr <- nrow(groups$row_groups)
  Nc <- nrow(groups$col_groups)
  switch (
    relative_to,
    assays = {
      out <- expanded
      list(
        assays = as.list(out[[".group_id"]]),
        rows = as.list(out[[".rows::.indices_group_id"]]),
        cols = as.list(out[[".cols::.indices_group_id"]]),
        .nrow = as.list(out[[".nrows"]]),
        .ncol = as.list(out[[".ncols"]]),
        .nsize = as.list(out[[".nrows"]] * out[[".ncols"]])
      )
    },
    rows = {
      g_row <- groups$row_groups
      g_ind <- as.list(g_row[[".indices_group_id"]])
      c_ind <- groups$col_groups[[".indices_group_id"]]
      r_nrow <- vapply(g_row[[".indices"]], length, 1L)
      r_ncol <- vapply(groups$col_groups[[".indices"]], length, 1L)
      r_ncol <- vec_rep(sum(r_ncol), Nr)
      r_nsiz <- r_nrow * r_ncol
      list(
        assays = lapply(g_ind, function(i) Nr*(i-1L) + c_ind),
        rows = g_ind,
        cols = vec_rep(list(c_ind), Nr),
        .nrow = as.list(r_nrow),
        .ncol = as.list(r_ncol),
        .nsize = as.list(r_nsiz)
      )
    },
    cols = {
      g_col <- groups$col_groups
      g_ind <- as.list(g_col[[".indices_group_id"]])
      r_ind <- groups$row_groups[[".indices_group_id"]]
      c_ncol <- vapply(g_col[[".indices"]], length, 1L)
      c_nrow <- vapply(groups$row_groups[[".indices"]], length, 1L)
      c_nrow <- vec_rep(sum(c_nrow), Nc) #?
      c_nsiz <- c_nrow * c_ncol
      list(
        assays = lapply(g_ind, function(i) Nr*(r_ind-1L) + i),
        rows = vec_rep(list(r_ind), Nc),
        cols = g_ind,
        .nrow = as.list(c_nrow),
        .ncol = as.list(c_ncol),
        .nsize = as.list(c_nsiz)
      )
    }
  )
}

biocmask_group_ids <- function(.data, var) {
  group_by(.data, {{ var }}) |>
    mutate(rows_keep = !duplicated(.rows_group_id),
           cols_keep = !duplicated(.cols_group_id)) |>
    summarise(assays = list(.group_id[rows_keep | cols_keep]),
              rows = list(.rows_group_id[rows_keep]),
              cols = list(.cols_group_id[cols_keep]),
              .nrow = list(sum(.nrows[rows_keep])),
              .ncol = list(sum(.ncols[cols_keep])),
              .nsize = list(.nrow[[1]]*.ncol[[1]])) |>
    select(- {{ var }}) |>
    as.list()
}

env_group_id <- function(env) {
  force(env)
  function(id) {
    env[["biocmask:::ctx:::group_id"]] <- id
    invisible(id)
  }
}

prepare_shared_ctx_env <- function(groups, expanded) {
  # browser()
  ind_d <- attr(groups, "obj_dim")
  
  inf_assay <- biocmask_group_ids2(groups, expanded, "assays")
  inf_rows <- biocmask_group_ids2(groups, expanded, "rows")
  inf_cols <- biocmask_group_ids2(groups, expanded, "cols")

  assay_group_id = list(
    assays = inf_assay[["assays"]],
    rows = inf_rows[["assays"]],
    cols = inf_cols[["assays"]]
  )
  rows_group_id = list(
    assays = inf_assay[["rows"]],
    rows = inf_rows[["rows"]],
    cols = inf_cols[["rows"]]
  )
  cols_group_id = list(
    assays = inf_assay[["cols"]],
    rows = inf_rows[["cols"]],
    cols = inf_cols[["cols"]]
  )
  ctx_group_id = list(
    assays = inf_assay[["assays"]],
    rows = inf_rows[["rows"]],
    cols = inf_cols[["cols"]]
  )

  nrow_info = list(
    assays = inf_assay[[".nrow"]],
    rows = inf_rows[[".nrow"]],
    cols = inf_cols[[".nrow"]]
  )
  ncol_info = list(
    assays = inf_assay[[".ncol"]],
    rows = inf_rows[[".ncol"]],
    cols = inf_cols[[".ncol"]]
  )
  nsize_info = list(
    assays = inf_assay[[".nsize"]],
    rows = inf_rows[[".nsize"]],
    cols = inf_cols[[".nsize"]]
  )
  nsize_ctx = list(
    assays = inf_assay[[".nsize"]],
    rows = inf_rows[[".nrow"]],
    cols = inf_cols[[".ncol"]]
  )

  shared_ctx_env <- new_environment(
    data = list(
      `biocmask:::ctx` = "assays",
      `biocmask:::ctx:::group_id` = 1L,
      `biocmask:::assays:::group_chop_ids` = assay_group_id,
      `biocmask:::rows:::group_chop_ids` =   rows_group_id,
      `biocmask:::cols:::group_chop_ids` =   cols_group_id,
      `biocmask:::ctx:::group_chop_ids`  =   ctx_group_id,
      `biocmask:::dim:::nrow` = nrow_info,
      `biocmask:::dim:::ncol` = ncol_info,
      `biocmask:::dim:::size` = nsize_info,
      `biocmask:::dim:::n` =    nsize_ctx,
      `biocmask:::ctx:::n_groups` = expanded |>
        summarise(assays = max(.group_id),
                  rows = max(`.rows::.indices_group_id`),
                  cols = max(`.cols::.indices_group_id`)) |>
        as.list()
    ),
    parent = bot_env
  )
  
  shared_ctx_env$n <- new_function(pairlist(),quote(`biocmask:::ctx:::n`), shared_ctx_env)

  env_bind_active(
    shared_ctx_env,
    `biocmask:::n_groups` = new_function(
      pairlist(),
      expr(.subset2(`biocmask:::ctx:::n_groups`, `biocmask:::ctx`)),
      env = shared_ctx_env),
    `biocmask:::ctx:::current_chops` = new_function(
      pairlist(),
      # switch(`biocmask:::ctx`,
      #        assays = `biocmask:::assays:::current_chops`,
      #        rows = `biocmask:::rows:::current_chops`,
      # )
      expr(.subset2(`biocmask:::ctx:::group_chop_ids`, `biocmask:::ctx`) |>
             .subset2(`biocmask:::ctx:::group_id`)),
      env = shared_ctx_env
    ),
    `biocmask:::assays:::current_chops` = new_function(
      pairlist(),
      expr(.subset2(`biocmask:::assays:::group_chop_ids`, `biocmask:::ctx`) |>
             .subset2(`biocmask:::ctx:::group_id`)),
      env = shared_ctx_env
    ),
    `biocmask:::rows:::current_chops` = new_function(
      pairlist(),
      expr(.subset2(`biocmask:::rows:::group_chop_ids`, `biocmask:::ctx`) |>
             .subset2(`biocmask:::ctx:::group_id`)),
      env = shared_ctx_env
    ),
    `biocmask:::cols:::current_chops` = new_function(
      pairlist(),
      expr(.subset2(`biocmask:::cols:::group_chop_ids`, `biocmask:::ctx`) |>
             .subset2(`biocmask:::ctx:::group_id`)),
      env = shared_ctx_env
    ),
    `biocmask:::ctx:::nrow` = new_function(
      pairlist(),
      expr(.subset2(`biocmask:::dim:::nrow`, `biocmask:::ctx`) |>
             .subset2(`biocmask:::ctx:::group_id`)),
      env = shared_ctx_env
    ),
    `biocmask:::ctx:::ncol` = new_function(
      pairlist(),
      expr(.subset2(`biocmask:::dim:::ncol`, `biocmask:::ctx`) |>
             .subset2(`biocmask:::ctx:::group_id`)),
      env = shared_ctx_env
    ),
    `biocmask:::ctx:::size` = new_function(
      pairlist(),
      expr(.subset2(`biocmask:::dim:::size`, `biocmask:::ctx`) |>
             .subset2(`biocmask:::ctx:::group_id`)),
      env = shared_ctx_env
    ),
    `biocmask:::ctx:::n` = new_function(
      pairlist(),
      expr(.subset2(`biocmask:::dim:::n`, `biocmask:::ctx`) |>
             .subset2(`biocmask:::ctx:::group_id`)),
      env = shared_ctx_env
    )
  )
  shared_ctx_env$set_group_id <- env_group_id(shared_ctx_env)
  shared_ctx_env
}

# shared_ctx_env <- prepare_shared_ctx_env(ind)
#
# shared_ctx_env$`biocmask:::ctx` <- "rows"
# set_group_id(1L)
# shared_ctx_env$`biocmask:::ctx:::current_chops`
# shared_ctx_env$`biocmask:::assays:::current_chops`
# shared_ctx_env$`biocmask:::cols:::current_chops`
# shared_ctx_env$`biocmask:::rows:::current_chops`


# base_minimal <- new_environment(
#   data = list(
#     ## Syntax
#     `{` = base::`{`,
#     `(` = base::`(`,
#     `~` = base::`~`,
#     `<-` = base::`<-`,
#     `=` = base::`=`,
#     ##extractors
#     `[` = base::`[`,
#     `[[` = base::`[[`,
#     `$` = base::`$`,
#     `@` = base::`@`,
#     ## operators - logical
#     `|` = base::`|`,
#     `&` = base::`&`,
#     `||` = base::`||`,
#     `&&` = base::`&&`,
#     ## operators - math
#     `+` = base::`+`,
#     `-` = base::`-`,
#     `/` = base::`/`,
#     `*` = base::`*`,
#     `^` = base::`^`,
#     `sqrt` = base::sqrt,
#
#   )
# )
