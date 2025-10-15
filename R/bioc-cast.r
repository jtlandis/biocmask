#' @include bioc-ptype.R
#' @include bioc-ptype2.R
#' @include bioc-init.r

bioc_cast <- S7::new_generic(
  "bioc_cast",
  dispatch_args = c("from", "to"),
  function(from, to, ...) {
    S7_dispatch()
  }
)

S7::method(
  bioc_cast,
  signature = list(
    from = S7::class_any,
    to = NULL
  )
) <- function(from,
              to,
              ...) {
  NULL
}

S7::method(
  bioc_cast,
  signature = list(
    from = NULL,
    to = S7::class_any
  )
) <- function(from,
              to,
              ...) {
  bioc_ptype(to, ...)
}

S7::method(
  bioc_cast,
  signature = list(
    from = S7::class_any,
    to = S7::class_any
  )
) <- function(from,
              to,
              ...) {
  rlang::abort(
    sprintf(
      "cannot cast class <%s> into <%s>",
      paste0(class(from), collapse = "/"),
      paste0(class(to), collapse = "/")
    )
  )
}

S7::method(
  bioc_cast,
  signature = list(from = class_vec, to = class_vec)
) <- function(from,
              to,
              ...) {
  vctrs::vec_cast(
    x = from,
    to = to,
    ...,
    x_arg = "from",
    to_arg = "to",
    call = caller_env()
  )
}

S7::method(
  bioc_cast,
  signature = list(from = class_vec, to = class_vctrs_vec)
) <- function(from,
              to,
              ...) {
  vctrs::vec_cast(
    x = from,
    to = to,
    ...,
    x_arg = "from",
    to_arg = "to",
    call = caller_env()
  )
}


S7::method(
  bioc_cast,
  signature = list(from = class_vctrs_vec, to = class_vec)
) <- function(from,
              to,
              ...) {
  vctrs::vec_cast(
    x = from,
    to = to,
    ...,
    x_arg = "from",
    to_arg = "to",
    call = caller_env()
  )
}

S7::method(
  bioc_cast,
  signature = list(from = class_vctrs_vec, to = class_vctrs_vec)
) <- function(from,
              to,
              ...) {
  vctrs::vec_cast(
    x = from,
    to = to,
    ...,
    x_arg = "from",
    to_arg = "to",
    call = caller_env()
  )
}

S7::method(
  bioc_cast,
  signature = list(from = class_df, to = class_df)
) <- function(from,
              to,
              ...) {
  vctrs::vec_cast(
    x = from,
    to = to,
    ...,
    x_arg = "from",
    to_arg = "to",
    call = caller_env()
  )
}

bioc_cast_DF <- function(from,
                         to,
                         ...,
                         size = bioc_size(from)) {
  out <- bioc_ptype(to, x_arg = "y", call = caller_env()) |>
    as.list()
  xnames <- names(from)
  ynames <- names(out)
  force(size)
  if (!all(xnames %in% ynames)) {
    rlang::abort(
      sprintf(
        "cannot cast a <%s> with columns (%s) to a <%s> with columns (%s)",
        paste(class(from), collapse = "/"),
        paste(xnames, collapse = ", "),
        paste(class(to), collapse = "/"),
        paste(ynames, collapse = ", ")
      )
    )
  }
  for (name in xnames) {
    out[[name]] <- bioc_cast(from[[name]], out[[name]])
  }
  ynames <- setdiff(ynames, xnames)
  for (name in ynames) {
    out[[name]] <- bioc_init(out[[name]], size = size)
  }
  new_DF(out, nrows = size, rownames = rownames(from))
}

S7::method(
  bioc_cast,
  signature = list(from = class_DF, to = class_df)
) <- bioc_cast_DF

S7::method(
  bioc_cast,
  signature = list(from = class_df, to = class_DF)
) <- bioc_cast_DF

S7::method(
  bioc_cast,
  signature = list(from = class_DF, to = class_DF)
) <- bioc_cast_DF

bioc_cast_mcols <- function(from, to) {
  S4Vectors::mcols(from) <- bioc_cast_DF(S4Vectors::mcols(from), S4Vectors::mcols(to), size = bioc_size(from))
  from
}

S7::method(
  bioc_cast,
  signature = list(from = class_s4_vec, to = class_s4_vec)
) <- function(from, to, ...) {
  bioc_cast_mcols(methods::as(from, class(to)), to = to)
}

S7::method(
  bioc_cast,
  signature = list(from = class_vec, to = class_s4_vec)
) <- function(from, to, ...) {
  bioc_cast_mcols(methods::as(from, class(to)), to = to)
}

S7::method(
  bioc_cast,
  signature = list(from = class_s4_vec, to = class_vec)
) <- function(from, to, ...) {
  methods::as(from, class(to))
}

# attempt_ptype2 <- function(x, y) {
#   out <- c(x, y)
#   out[0]
# }

# S7::method(
#   bioc_ptype2,
#   signature = list(x = class_s4_vec, y = class_s4_vec),
#   def = function(x,
#                  y,
#                  ...) {
#     attempt_ptype2(x, y)
#   }
# )

# S7::method(
#   bioc_ptype2,
#   signature = list(x = class_vec, y = class_s4_vec),
#   def = function(x,
#                  y,
#                  ...) {
#     attempt_ptype2(x, y)
#   }
# )

# S7::method(
#   bioc_ptype2,
#   signature = list(x = class_s4_vec, y = class_vec),
#   def = function(x,
#                  y,
#                  ...) {
#     attempt_ptype2(x, y)
#   }
# )
