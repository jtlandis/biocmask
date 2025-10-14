#' bioc_init
#' @description
#' create an object of some size.
#' @param x the prototype object. typically unused
#' @param size the size of the object to create
#' @param ... unused
#' @export
setGeneric(
  "bioc_init",
  signature = "x",
  function(x, size = 0L, ...) {
    # this is very undesirable as we cannot assume x is a
    # "vctrs_vctr" class, and S4 does not dispatch against S3
    # subclasses
    if (inherits(x, "vctrs_vctr")) {
      obj <- bioc_init(vctrs::vec_proxy(x), size = size)
      return(vctrs::vec_restore(obj, x))
    }
    standardGeneric("bioc_init")
  }
)

setMethod(
  "bioc_init",
  signature = "ANY",
  function(x, size = 0L, ...) {
    rlang::abort(
      sprintf(
        "No method to create <%s> of size %i",
        paste0(class(x), collapse = "/"),
        as.integer(size)
      )
    )
  }
)

setMethod(
  "bioc_init",
  signature = "character",
  function(x, size = 0L, ...) {
    character(size)
  }
)

setMethod(
  "bioc_init",
  signature = "double",
  function(x, size = 0L, ...) {
    double(size)
  }
)

setMethod(
  "bioc_init",
  signature = "integer",
  function(x, size = 0L, ...) {
    integer(size)
  }
)

setMethod(
  "bioc_init",
  signature = "numeric",
  function(x, size = 0L, ...) {
    numeric(size)
  }
)

setMethod(
  "bioc_init",
  signature = "logical",
  function(x, size = 0L, ...) {
    logical(size)
  }
)

setMethod(
  "bioc_init",
  signature = "complex",
  function(x, size = 0L, ...) {
    complex(size)
  }
)

setMethod(
  "bioc_init",
  signature = "list",
  function(x, size = 0L, ...) {
    vector("list", size)
  }
)

setMethod(
  "bioc_init",
  signature = list(getClass("List", where = "S4Vectors")),
  function(x, size = 0L, ...) {
    S4Vectors::SimpleList(vector("list", size))
  }
)

setMethod(
  "bioc_init",
  "factor",
  function(x, size = 0L, ...) {
    f <- rep(NA_integer_, size)
    levels(f) <- levels(x)
    class(f) <- "factor"
    f
  }
)

setMethod(
  "bioc_init",
  "ordered",
  function(x, size = 0L, ...) {
    f <- rep(NA_integer_, size)
    levels(f) <- levels(x)
    class(f) <- c("ordered", "factor")
    f
  }
)

setMethod(
  "bioc_init",
  signature = list(getClass("Rle", where = "S4Vectors")),
  function(x, size = 0L, ...) {
    S4Vectors::Rle(
      values = bioc_init(S4Vectors::values(x), size = size)
    )
  }
)

setMethod(
  "bioc_init",
  signature = "data.frame",
  function(x, size = 0L, ...) {
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
)
