#' bioc_init
#' @description
#' create an object of some size.
#' @param x the prototype object. typically unused
#' @param size the size of the object to create
#' @param ... unused
#' @export
bioc_init <- S7::new_generic(
  "bioc_init",
  dispatch_args = "x",
  function(x, size = 0L, ...) {
    # # this is very undesirable as we cannot assume x is a
    # # "vctrs_vctr" class, and S4 does not dispatch against S3
    # # subclasses
    # if (inherits(x, "vctrs_vctr")) {
    #   obj <- bioc_init(vctrs::vec_proxy(x), size = size)
    #   return(vctrs::vec_restore(obj, x))
    # }
    S7_dispatch()
  }
)

S7::method(
  bioc_init,
  signature = S7::class_any
) <- function(x, size = 0L, ...) {
  rlang::abort(
    sprintf(
      "No method to create <%s> of size %i",
      paste0(class(x), collapse = "/"),
      as.integer(size)
    )
  )
}

S7::method(
  bioc_init,
  signature = S7::class_character
) <- function(x, size = 0L, ...) {
  character(size)
}

S7::method(
  bioc_init,
  signature = S7::class_double
) <- function(x, size = 0L, ...) {
  double(size)
}

S7::method(
  bioc_init,
  signature = S7::class_integer
) <- function(x, size = 0L, ...) {
  integer(size)
}


S7::method(
  bioc_init,
  signature = S7::class_logical
) <- function(x, size = 0L, ...) {
  logical(size)
}

S7::method(
  bioc_init,
  signature = S7::class_complex
) <- function(x, size = 0L, ...) {
  complex(size)
}

S7::method(
  bioc_init,
  signature = S7::class_list
) <- function(x, size = 0L, ...) {
  vector("list", size)
}

S7::method(
  bioc_init,
  signature = getClass("List", where = "S4Vectors")
) <- function(x, size = 0L, ...) {
  S4Vectors::SimpleList(vector("list", size))
}

S7::method(
  bioc_init,
  S7::class_factor
) <- function(x, size = 0L, ...) {
  f <- rep(NA_integer_, size)
  levels(f) <- levels(x)
  class(f) <- "factor"
  f
}


S7::method(
  bioc_init,
  S7::new_S3_class(c("ordered", "factor"))
) <- function(x, size = 0L, ...) {
  f <- rep(NA_integer_, size)
  levels(f) <- levels(x)
  class(f) <- c("ordered", "factor")
  f
}

S7::method(
  bioc_init,
  signature = getClass("Rle", where = "S4Vectors")
) <- function(x, size = 0L, ...) {
  S4Vectors::Rle(
    values = bioc_init(S4Vectors::values(x), size = size)
  )
}

S7::method(
  bioc_init,
  signature = class_df
) <- function(x, size = 0L, ...) {
  df <- vector("list", ncol(x))
  names(df) <- names(x)
  for (name in names(df)) {
    df[[name]] <- bioc_init(x[[name]], size = size)
  }
  .rownames <- attr(x, "row.names")
  attr(df, "row.names") <- if (is.null(.rownames) ||
    length(.rownames) != size) {
    .set_row_names(size)
  } else {
    .rownames
  }
  class(df) <- "data.frame"
  df
}
