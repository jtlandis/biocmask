
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
    vec_rep = vctrs::vec_rep,
    vec_rep_each = vctrs::vec_rep_each,
    vec_c = vctrs::vec_c,
    skip = skip,
    poke_ctx = poke_ctx,
    poke_ctx_local = poke_ctx_local,
    peek_ctx = peek_ctx,
    .mask_manager = NULL,
    rows = function(...) {
      # browser()
      mask_manager <- peek_ctx("SE:::mask_manager")
      fn <- peek_ctx("SE:::dplyr_function")
      env <- peek_ctx("SE:::caller_env")
      eval_fun <- switch(fn,
                         mutate = mask_manager$eval_mutate_rows,
                         stop(sprintf("`%s` is not yet implemented", fn)))
      quos <- enquos(...)
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
                         stop(sprintf("`%s` is not yet implemented", fn)))
      quos <- enquos(...)
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
