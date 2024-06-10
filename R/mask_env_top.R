
ctx_env <- new.env(parent = emptyenv())

peek_ctx <- function(name) {
  ctx_env[[name]]
}
poke_ctx_local <- function(name, value) {
  old <- ctx_env[[name]]
  ctx_env[[name]] <- value
  quo <- expr(ctx_env[[name]] <- !!old)
  do.call(
    on.exit,
    list(quo, add = TRUE),
    env = parent.frame()
  )
  invisible(old)
}


top_env <- new_environment(
  data = list(
    poke_ctx = poke_ctx,
    poke_ctx_local = poke_ctx_local,
    peek_ctx = peek_ctx,
    .mask_manager = NULL,
    rows = function(...) {
      mask_manager <- peek_ctx("SE:::mask_manager")
      fn <- peek_ctx("SE:::dplyr_function")
      eval_fun <- switch(fn,
                         mutate = mask_manager$eval_mutate_rows,
                         stop(sprintf("`%s` is not yet implemented", fn)))
      quos <- enquos(...)
      nms <- names(quos)
      for (i in seq_along(quos)) {
        quo <- quos[[i]]
        name <- nms[i]
        eval_fun(quo, name)
      }
      
    },
    cols = function(...) {
      mask_manager <- peek_ctx("SE:::mask_manager")
      fn <- peek_ctx("SE:::dplyr_function")
      eval_fun <- switch(fn,
                         mutate = mask_manager$eval_mutate_cols,
                         stop(sprintf("`%s` is not yet implemented", fn)))
      # eval_mask <- peek_ctx("rowData_mask")
      # call_env <- peek_ctx("caller_env")
      quos <- enquos(...)
      nms <- names(quos)
      for (i in seq_along(quos)) {
        quo <- quos[[i]]
        name <- nms[i]
        eval_fun(quo, name)
      }
      
    }
  )
)


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
