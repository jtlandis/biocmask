#' @name bioc_ptype
#' @description
#' get the zero size prototype of an object
#' @return an object of size 0
#' @export
bioc_ptype2 <- S7::new_generic(
  "bioc_ptype2",
  dispatch_args = c("x", "y"),
  function(x, y, ...) {
    S7_dispatch()
  }
)

S7::method(
  bioc_ptype2,
  signature = list(x = S7::class_any, y = NULL)
) <- function(x,
              y,
              ...) {
  bioc_ptype(x, ...)
}


S7::method(
  bioc_ptype2,
  signature = list(x = NULL, y = S7::class_any)
) <- function(x,
              y,
              ...) {
  bioc_ptype(y, ...)
}


S7::method(
  bioc_ptype2,
  signature = list(x = S7::class_any, y = S7::class_any)
) <- function(x,
              y,
              ...) {
  rlang::abort(
    sprintf(
      "cannot combind class <%s> with <%s>",
      paste0(class(x), collapse = "/"),
      paste0(class(y), collapse = "/")
    )
  )
}


S7::method(
  bioc_ptype2,
  signature = list(x = class_vec, y = class_vec)
) <- function(x,
              y,
              ...) {
  vctrs::vec_ptype2(
    x = x,
    y = y,
    ...,
    x_arg = "x",
    y_arg = "y",
    call = caller_env()
  )
}


S7::method(
  bioc_ptype2,
  signature = list(x = class_vec, y = class_vctrs_vec)
) <- function(x,
              y,
              ...) {
  vctrs::vec_ptype2(
    x = x,
    y = y,
    ...,
    x_arg = "x",
    y_arg = "y",
    call = caller_env()
  )
}


S7::method(
  bioc_ptype2,
  signature = list(x = class_vctrs_vec, y = class_vec)
) <- function(x,
              y,
              ...) {
  vctrs::vec_ptype2(
    x = x,
    y = y,
    ...,
    x_arg = "x",
    y_arg = "y",
    call = caller_env()
  )
}

S7::method(
  bioc_ptype2,
  signature = list(x = class_vctrs_vec, y = class_vctrs_vec)
) <- function(x,
              y,
              ...) {
  vctrs::vec_ptype2(
    x = x,
    y = y,
    ...,
    x_arg = "x",
    y_arg = "y",
    call = caller_env()
  )
}


S7::method(
  bioc_ptype2,
  signature = list(x = class_df, y = class_df)
) <- function(x,
              y,
              ...) {
  vctrs::vec_ptype2(
    x = x,
    y = y,
    ...,
    x_arg = "x",
    y_arg = "y",
    call = caller_env()
  )
}


S7::method(
  bioc_ptype2,
  signature = list(x = class_DF, y = class_df)
) <- function(x,
              y,
              ...) {
  x <- slice_DF(x, 0L)
  y <- vctrs::vec_ptype(y, x_arg = "y", call = caller_env())
  col_names <- union(names(x), names(y))
  names(col_names) <- col_names
  lapply(
    col_names,
    function(name, x, y) {
      bioc_ptype2(x[[name]], y[[name]])
    },
    x = x,
    y = y
  ) |>
    new_DF(nrows = 0L)
}


S7::method(
  bioc_ptype2,
  signature = list(x = class_df, y = class_DF)
) <- function(x,
              y,
              ...) {
  x <- vctrs::vec_ptype(x, x_arg = "x", call = caller_env())
  y <- slice_DF(y, 0L)
  col_names <- union(names(x), names(y))
  names(col_names) <- col_names
  lapply(
    col_names,
    function(name, x, y) {
      bioc_ptype2(x[[name]], y[[name]])
    },
    x = x,
    y = y
  ) |>
    new_DF(nrows = 0L)
}


S7::method(
  bioc_ptype2,
  signature = list(x = class_DF, y = class_DF)
) <- function(x,
              y,
              ...) {
  x <- slice_DF(x, 0L)
  y <- slice_DF(y, 0L)
  col_names <- union(names(x), names(y))
  names(col_names) <- col_names
  lapply(
    col_names,
    function(name, x, y) {
      bioc_ptype2(x[[name]], y[[name]])
    },
    x = x,
    y = y
  ) |>
    new_DF(nrows = 0L)
}


attempt_ptype2 <- function(x, y) {
  out <- tryCatch(
    c(x, y),
    error = function(cnd) {
      rlang::abort(
        sprintf(
          "cannot combind class <%s> with <%s>",
          paste0(class(x), collapse = "/"),
          paste0(class(y), collapse = "/")
        ),
        parent = cnd
      )
    }
  )
  out <- bioc_slice(out, 0L)
  if (methods::is(out, class_s4_vec)) {
    S4Vectors::mcols(out) <- bioc_ptype2(
      S4Vectors::mcols(x),
      S4Vectors::mcols(y)
    )
  }
  out
}

S7::method(
  bioc_ptype2,
  signature = list(x = class_s4_vec, y = class_s4_vec)
) <- function(x,
              y,
              ...) {
  attempt_ptype2(x, y)
}

S7::method(
  bioc_ptype2,
  signature = list(x = class_vec, y = class_s4_vec)
) <- function(x,
              y,
              ...) {
  attempt_ptype2(x, y)
}


S7::method(
  bioc_ptype2,
  signature = list(x = class_s4_vec, y = class_vec)
) <- function(x,
              y,
              ...) {
  attempt_ptype2(x, y)
}



#' @export
bioc_ptype_common <- function(..., .ptype = NULL) {
  bioc_ptype_common_list(rlang::list2(...), .ptype = .ptype)
}

bioc_ptype_common_list <- function(dots, .ptype) {
  dots <- lapply(dots, bioc_slice, 0L)
  base::Reduce(bioc_ptype2, x = dots, init = .ptype)
}

#' @include bioc-unchop.r
#' concatinate objects
#' @description
#' an alternative to `base::c` and `vctrs::vec_c` that will
#' use `biocmask::bioc_unchop` under the hood.
#' @export
bioc_c <- function(...) {
  dots <- rlang::list2(...)
  ptype <- bioc_ptype_common_list(dots, NULL)
  dots <- lapply(dots, bioc_cast, to = ptype)
  bioc_unchop(x = dots, ptype = ptype)
}
