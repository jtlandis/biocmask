
ctx_quo <- function(expr, env, ctx) {
  quo <- rlang::new_quosure(expr = expr, env = env)
  attr(quo, "biocmask:::ctx") <- ctx
  quo
}

enforce_named <- function(exprs) {
  to_update <- FALSE
  nms <- names(exprs) %||% {to_update <- TRUE; vapply(exprs, rlang::as_label, "")}
  char_len <- nzchar(nms)
  if (any(to_rename <- char_len==0)) {
    nms[to_rename] <- vapply(exprs[to_rename], rlang::as_label, "")
    to_update <- TRUE
  }
  if (to_update) {
    names(exprs) <- nms
  }
  exprs
}

biocmask_quos <- function(..., env = parent.frame()) {
  # browser()
  dots <- rlang::enexprs(...)
  ctx_opt <- c("cols", "rows")
  dots
  for (i in seq_along(dots)) {
    if (rlang::is_call(dots[[i]], c("cols","rows"))) {
      ctx <- rlang::as_label(dots[[i]][[1]])
      
      dots[[i]] <- lapply(dots[[i]][-1],
                          ctx_quo,
                          env = env,
                          ctx = ctx) |>
        rlang::splice()
      next
    }
    dots[[i]] <- ctx_quo(dots[[i]], env = env, ctx = "assays")
  } 
  out <- do.call(rlang::list2, dots)
  enforce_named(out)
}

list3 <- function(...) {
  .Call("ffi_dots_list", frame_env = environment(), named = NULL, 
        ignore_empty = "trailing", preserve_empty = FALSE, unquote_names = TRUE, 
        homonyms = "keep", check_assign = FALSE,
        PACKAGE = "dplyr")
}
