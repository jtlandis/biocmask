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
  ),
  def = function(from,
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
)

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
    y_arg = "to",
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
    y_arg = "to",
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
    y_arg = "to",
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
    y_arg = "to",
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
    y_arg = "to",
    call = caller_env()
  )
}

bioc_cast_DF <- function(from,
                         to,
                         ...) {
  to <- bioc_ptype(to, x_arg = "y", call = caller_env()) |>
    as.list()
  xnames <- names(from)
  ynames <- names(to)
  if (!all(xnames %in% ynames)) {
    rlang::abort(
      sprintf(
        "cannot cast a DataFrame with columns (%s) to a data.frame with columns (%s)",
        paste(xnames, collapse = ", "),
        paste(ynames, collapse = ", ")
      )
    )
  }
  for (name in xnames) {
    to[[name]] <- bioc_cast(from[[name]], to[[name]])
  }
  ynames <- setdiff(ynames, xnames)
  size <- bioc_size(from)
  for (name in ynames) {
    to[[name]] <- bioc_init(to[[name]], size = size)
  }
  new_DF(to, nrows = size, rownames = rownames(from))
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

S7::method(
  bioc_cast,
  signature = list(from = class_s4_vec, y = class_s4_vec)
) <- function(from, to, ...) {
  methods::as(from, class(to))
}

S7::method(
  bioc_cast,
  signature = list(from = class_vec, y = class_s4_vec)
) <- function(from, to, ...) {
  methods::as(from, class(to))
}

S7::method(
  bioc_cast,
  signature = list(from = class_s4_vec, y = class_vec)
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

is_anno <- S7::new_generic("is_anno", "x")

S7::method(
  is_anno,
  getClass("Annotated")
) <- function(x, ...) {
  cat("this object is annotated\n")
  invisible(x)
}
