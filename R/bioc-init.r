#' bioc_init
#' @description
#' create an object of some size. This generic does not have an equivalent
#' function from the vctrs pacakge however it was created to support the
#' funcitonality of `bioc_cast()` between two data.frame-like objects.
#'
#' The default behavior of `vctrs::vec_cast(x, to)` when x is a data.frame and
#' and `to` is another, potentially bigger data.frame, is to fill in the missing
#' names with missing prototype vectors.
#'
#' The vctrs pacakge provides a way to potentially cover any arbitrary case if
#' both vctrs::vec_proxy and vctrs::vec_restore are defined for the class.
#'
#' However, this is not always the case for S4Vectors::Vectors and other S4
#' classes. Thus, `bioc_init()` was created to provide developers a way to
#' create another default instance of their classes if dispatched upon.
#'
#'
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
    size <- as.integer(size)
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
  signature = NULL
) <- function(x, size = 0L, ...) {
  NULL
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
  signature = class_vctrs_vec
) <- function(x, size = 0L, ...) {
  obj <- bioc_init(vctrs::vec_proxy(x), size = size)
  vctrs::vec_restore(obj, x)
}

attempt_s4_init <- function(x, size) {
  # browser()
  s4_class <- methods::getClass(class(x))
  # just look at the first union...
  while (methods::is(s4_class, "ClassUnionRepresentation")) {
    s4_class <- base::Filter(\(x) x@distance == 1L, s4_class@subclasses)[[1]]
  }
  class_name <- s4_class@className
  default_slots <- s4_class@slots
  parallel <- S4Vectors::parallel_slot_names(x)
  names(parallel) <- parallel

  browser()
  default_slots_cl <- lapply(default_slots, \(cl) {
    cl <- methods::getClass(cl)
    while (methods::is(cl, "ClassUnionRepresentation")) {
      cl <- base::Filter(\(x) x@distance == 1L, cl@subclasses)[[1]]
      if (methods::is(cl, "SClassExtension")) {
        cl <- methods::getClass(cl@subClass)
      }
    }
    cl
  })
  default_slots <- lapply(default_slots_cl, \(cl) {
    if (methods::isVirtualClass(cl)) {
      cl <- cl@subclasses[[1L]]
      if (methods::is(cl, "SClassExtension")) {
        cl <- methods::getClass(cl@subClass)
      }
    }
    methods::new(cl)
  })

  parallel_slots <- lapply(default_slots[parallel], bioc_init, size = size)
  default_slots <- default_slots[setdiff(names(default_slots), parallel)]
  do.call(
    methods::new,
    c(
      list(
        Class = class_name
      ),
      parallel_slots,
      default_slots
    )
  )
}

S7::method(
  bioc_init,
  signature = class_s4_vec
) <- function(x, size = 0L, ...) {
  tryCatch(
    attempt_s4_init(x = x, size = size),
    error = function(cnd) {
      failed_class <- paste0(class(x), collapse = "/")
      rlang::abort(
        c(
          sprintf(
            "failed to initialize object of class <%s> and size '%i'",
            failed_class,
            size
          ),
          i = sprintf(
            "consider creating a `bioc_init()` method for <%s>",
            failed_class
          )
        ),
        parent = cnd
      )
    }
  )
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
    values = bioc_init(S4Vectors::runValue(x), size = size)
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

S7::method(
  bioc_init,
  signature = class_DF
) <- function(x, size = 0L, ...) {
  df <- vector("list", ncol(x))
  names(df) <- names(x)
  for (name in names(df)) {
    df[[name]] <- bioc_init(x[[name]], size = size)
  }
  .rownames <- rownames(x)
  .rownames <- if (length(.rownames) != size) {
    NULL
  }
  new_DF(
    .data = df,
    rownames = .rownames,
    nrows = size
  )
}
