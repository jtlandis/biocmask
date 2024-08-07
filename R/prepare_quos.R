
ctx_quo <- function(expr, env, ctx) {
  quo <- new_quosure(expr = expr, env = env)
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

#' @title biocmask quosures
#' @description
#' a consistent way to handle `...` for dplyr extensions.
#' This returns a list of quosures where each quosure 
#' contains an attribute `biocmask:::ctx` indicating which
#' mask context it should be evaluate in.
#' @param ... rlang dots, supports splicing an quoting
#' @param .named should resulting expressions be named?
#' @noRd
biocmask_quos <- function(..., .named = TRUE) {
  # browser()
  dots <- quos(...) |>
    as.list()
  ctx_opt <- c("cols", "rows")
  for (i in seq_along(dots)) {
    quo <- dots[[i]]
    .env <- quo_get_env(quo)
    .expr <- quo_get_expr(quo)
    if (is_call(.expr, ctx_opt)) {
      ctx <- as_label(.expr[[1]])
      dots[[i]] <- lapply(.expr[-1],
                          ctx_quo,
                          env = .env,
                          ctx = ctx) |>
        splice()
      next
    }
    dots[[i]] <- ctx_quo(.expr, env = .env, ctx = "assays")
  } 
  out <- do.call(dots_list, c(dots, list(.named = .named)))
  if (.named) {
    # in case the prior expansion
    # of rows(...) and cols(...) need
    # a named argument
    out <- enforce_named(out)
  }
  out
}

enforce_matrix <- function(quos, ctxs) {
  is_assay_ctx <- ctxs == "assays"
  quos[is_assay_ctx] <- lapply(
    quos[is_assay_ctx],
    function(quo) {
      if (is.null(quo_get_expr(quo))) 
        return(quo)
      else
        quo_set_expr(quo,
                     expr(matrix(!!quo,
                                 nrow = `biocmask:::ctx:::nrow`,
                                 ncol = `biocmask:::ctx:::ncol`)))
      }
    )
  quos
}
