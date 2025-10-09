#' @name bioc_ptype
#' @description
#' get the zero size prototype of an object
#' @return an object of size 0
#' @export
setGeneric(
  "bioc_ptype2",
  signature = c("x", "y"),
  def = function(x, y, ...) {
    standardGeneric("bioc_ptype2")
  }
)

setMethod(
  "bioc_ptype2",
  signature = list(x = methods::getClass("ANY"), y = methods::getClass("NULL")),
  def = function(x,
                 y,
                 ...) {
    bioc_ptype(x, ...)
  }
)

setMethod(
  "bioc_ptype2",
  signature = list(x = methods::getClass("NULL"), y = methods::getClass("ANY")),
  def = function(x,
                 y,
                 ...) {
    bioc_ptype(y, ...)
  }
)

setMethod(
  "bioc_ptype2",
  signature = list(x = methods::getClass("ANY"), y = methods::getClass("ANY")),
  def = function(x,
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
)

setMethod(
  "bioc_ptype2",
  signature = list(x = class_vec, y = class_vec),
  def = function(x,
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
)

setMethod(
  "bioc_ptype2",
  signature = list(x = class_vec, y = class_vctrs_vec),
  def = function(x,
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
)

setMethod(
  "bioc_ptype2",
  signature = list(x = class_vctrs_vec, y = class_vec),
  def = function(x,
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
)

setMethod(
  "bioc_ptype2",
  signature = list(x = class_vctrs_vec, y = class_vctrs_vec),
  def = function(x,
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
)

setMethod(
  "bioc_ptype2",
  signature = list(x = class_df, y = class_df),
  def = function(x,
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
)

setMethod(
  "bioc_ptype2",
  signature = list(x = class_DF, y = class_df),
  def = function(x,
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
)

setMethod(
  "bioc_ptype2",
  signature = list(x = class_df, y = class_DF),
  def = function(x,
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
)

setMethod(
  "bioc_ptype2",
  signature = list(x = class_DF, y = class_DF),
  def = function(x,
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
)

attempt_ptype2 <- function(x, y) {
  out <- c(x, y)
  out[0]
}

setMethod(
  "bioc_ptype2",
  signature = list(x = class_s4_vec, y = class_s4_vec),
  def = function(x,
                 y,
                 ...) {
    attempt_ptype2(x, y)
  }
)

setMethod(
  "bioc_ptype2",
  signature = list(x = class_vec, y = class_s4_vec),
  def = function(x,
                 y,
                 ...) {
    attempt_ptype2(x, y)
  }
)

setMethod(
  "bioc_ptype2",
  signature = list(x = class_s4_vec, y = class_vec),
  def = function(x,
                 y,
                 ...) {
    attempt_ptype2(x, y)
  }
)


#' @export
bioc_ptype_common <- function(..., .ptype = NULL) {
  bioc_ptype_common_list(rlang::list2(...), .ptype = .ptype)
}

bioc_ptype_common_list <- function(dots, .ptype) {
  dots <- lapply(dots, bioc_slice, 0L)
  base::Reduce(bioc_ptype2, x = dots, init = .ptype)
}

#' concatinate objects
#' @description
#' an alternative to `base::c` and `vctrs::vec_c` that will
#' use `biocmask::bioc_unchop` under the hood.
#' @export
bioc_c <- function(...) {
  dots <- rlang::list2(...)
  bioc_unchop(x = dots, ptype = bioc_ptype_common_list(dots, NULL))
}
