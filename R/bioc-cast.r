#' @include bioc-ptype.R
#' @include bioc-ptype2.R
#' @include bioc-init.r

setGeneric(
  "bioc_cast",
  signature = c("from", "to"),
  function(from, to, ...) {
    standardGeneric("bioc_cast")
  }
)

setMethod(
  "bioc_cast",
  signature = list(
    from = methods::getClass("ANY"),
    to = methods::getClass("NULL")
  ),
  def = function(from,
                 to,
                 ...) {
    NULL
  }
)

setMethod(
  "bioc_cast",
  signature = list(
    from = methods::getClass("NULL"),
    to = methods::getClass("ANY")
  ),
  def = function(from,
                 to,
                 ...) {
    bioc_ptype(to, ...)
  }
)

setMethod(
  "bioc_cast",
  signature = list(
    from = methods::getClass("ANY"),
    to = methods::getClass("ANY")
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

setMethod(
  "bioc_cast",
  signature = list(from = class_vec, to = class_vec),
  def = function(from,
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
)

setMethod(
  "bioc_cast",
  signature = list(from = class_vec, to = class_vctrs_vec),
  def = function(from,
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
)


setMethod(
  "bioc_cast",
  signature = list(from = class_vctrs_vec, to = class_vec),
  def = function(from,
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
)

setMethod(
  "bioc_cast",
  signature = list(from = class_vctrs_vec, to = class_vctrs_vec),
  def = function(from,
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
)

setMethod(
  "bioc_cast",
  signature = list(from = class_df, to = class_df),
  def = function(from,
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
)

setMethod(
  "bioc_cast",
  signature = list(from = class_DF, to = class_df),
  def = function(from,
                 to,
                 ...) {
    to <- vctrs::vec_ptype(to, x_arg = "y", call = caller_env()) |>
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
)

setMethod(
  "bioc_cast",
  signature = list(from = class_df, to = class_DF),
  def = function(from,
                 to,
                 ...) {
    to <- vctrs::vec_ptype(to, x_arg = "y", call = caller_env()) |>
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
)

setMethod(
  "bioc_cast",
  signature = list(from = class_DF, to = class_DF),
  def = function(from,
                 to,
                 ...) {
    to <- vctrs::vec_ptype(to, x_arg = "y", call = caller_env()) |>
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
)

setMethod(
  "bioc_cast",
  signature = list(from = class_s4_vec, y = class_s4_vec),
  def = function(from, to, ...) {
    methods::as(from, class(to))
  }
)

setMethod(
  "bioc_cast",
  signature = list(from = class_vec, y = class_s4_vec),
  def = function(from, to, ...) {
    methods::as(from, class(to))
  }
)

setMethod(
  "bioc_cast",
  signature = list(from = class_s4_vec, y = class_vec),
  def = function(from, to, ...) {
    methods::as(from, class(to))
  }
)

# attempt_ptype2 <- function(x, y) {
#   out <- c(x, y)
#   out[0]
# }

# setMethod(
#   "bioc_ptype2",
#   signature = list(x = class_s4_vec, y = class_s4_vec),
#   def = function(x,
#                  y,
#                  ...) {
#     attempt_ptype2(x, y)
#   }
# )

# setMethod(
#   "bioc_ptype2",
#   signature = list(x = class_vec, y = class_s4_vec),
#   def = function(x,
#                  y,
#                  ...) {
#     attempt_ptype2(x, y)
#   }
# )

# setMethod(
#   "bioc_ptype2",
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
