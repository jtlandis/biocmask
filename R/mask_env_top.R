
`skip!` <- structure(list(), class = "skip")
skip <- function() {
  `skip!`
}
print.skip <- function(x, ...) cat("<skip>\n")
is_skip <- function(x) inherits(x, "skip")


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
    vec_chop_assays_row.matrix = vec_chop_assays_row.matrix,
    vec_chop_assays_row.vctrs_grouped_list = vec_chop_assays_row.vctrs_grouped_list,
    vec_chop_assays_col = vec_chop_assays_col,
    vec_chop_assays_col.matrix = vec_chop_assays_col.matrix,
    vec_chop_assays_col.vctrs_grouped_list = vec_chop_assays_col.vctrs_grouped_list,
    wrap = wrap,
    wrap.default = wrap.default,
    wrap.vctrs_grouped_list = wrap.vctrs_grouped_list,
    # new_grouped_lst = new_grouped_lst,
    vec_rep = vctrs::vec_rep,
    vec_rep_each = vctrs::vec_rep_each,
    vec_c = vctrs::vec_c,
    skip = skip,
    poke_ctx = poke_ctx,
    poke_ctx_local = poke_ctx_local,
    peek_ctx = peek_ctx,
    .current_group_id = 0L,
    .mask_manager = NULL,
    rows = function(...) {
      # browser()
      mask_manager <- peek_ctx("SE:::mask_manager")
      fn <- peek_ctx("SE:::dplyr_function")
      env <- peek_ctx("SE:::caller_env")
      eval_fun <- switch(fn,
                         mutate = mask_manager$eval_mutate_rows,
                         group_by = mask_manager$eval_mutate_rows,
                         stop(sprintf("`%s` is not yet implemented", fn)))
      quos <- enquos(..., .named = TRUE)
      nms <- names(quos)
      for (i in seq_along(quos)) {
        quo <- rlang::quo_set_env(quos[[i]], env)
        name <- nms[i]
        eval_fun(quo, name)
      }
      skip()
    },
    cols = function(...) {
      mask_manager <- peek_ctx("SE:::mask_manager")
      fn <- peek_ctx("SE:::dplyr_function")
      env <- peek_ctx("SE:::caller_env")
      eval_fun <- switch(fn,
                         mutate = mask_manager$eval_mutate_cols,
                         group_by = mask_manager$eval_mutate_cols,
                         stop(sprintf("`%s` is not yet implemented", fn)))
      quos <- enquos(..., .named = TRUE)
      nms <- names(quos)
      for (i in seq_along(quos)) {
        quo <- rlang::quo_set_env(quos[[i]], env)
        name <- nms[i]
        eval_fun(quo, name)
      }
      skip()
    }
  ),
  parent = baseenv()
)

bot_env <- new.env(parent = top_env)

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

prepare_shared_ctx_env <- function(ind) {
  ind_d <- attr(ind, "obj_dim")
  # ind <- mutate(
  #   ind,
  #   .nrows = vapply(.rows, length, integer(1)),
  #   .ncols = vapply(.cols, length, integer(1)),
  #   .nrows = case_when(.nrows==0 ~ .env$ind_d[[1]],
  #                      TRUE ~ .nrows),
  #   .ncols = case_when(.ncols==0 ~ .env$ind_d[[2]],
  #                      TRUE ~ .ncols)
  # )
  inf_assay <- biocmask_group_ids(ind, .group_id)
  inf_rows <- biocmask_group_ids(ind, .rows_group_id)
  inf_cols <- biocmask_group_ids(ind, .cols_group_id)

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
      `biocmask:::ctx:::n_groups` = ind |>
        summarise(assays = max(.group_id),
                  rows = max(.rows_group_id),
                  cols = max(.cols_group_id)) |>
        as.list()
    ),
    parent = bot_env
  )

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
