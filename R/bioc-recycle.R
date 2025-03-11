#' @title Recycle a vector
#' @name vctrs-bioc_recycle
#' @description
#' A re-export of [`vctrs::vec_recycle`][vctrs::vec_recycle] as an S7 generic
#' function to allow `S4Vectors`.
#' @inheritParams vctrs::vec_recycle
#' @return a S3 or S4 vector
#' @examples
#' bioc_recycle(1L, size = 5L)
#' bioc_recycle(S4Vectors::Rle(1L), size = 5L)
#'
#' @export
setGeneric(
  "bioc_recycle",
  signature = "x",
  def = function(x, size, ...) {
    standardGeneric("bioc_recycle")
  }
)

setMethod(
  "bioc_recycle",
  signature = list(x = class_vec),
  def = function(
    x,
    size,
    ...
  ) {
    vctrs::vec_recycle(
      x = x,
      size = size,
      ...,
      x_arg = "x",
      call = caller_env()
    )
  }
)

setMethod(
  "bioc_recycle",
  signature = list(x = class_vctrs_vec),
  def = function(
    x,
    size,
    ...
  ) {
    vctrs::vec_recycle(
      x = x,
      size = size,
      ...,
      x_arg = "x",
      call = caller_env()
    )
  }
)

setMethod(
  "bioc_recycle",
  signature = list(x = class_df),
  def = function(
    x,
    size,
    ...
  ) {
    vctrs::vec_recycle(
      x = x,
      size = size,
      ...,
      x_arg = "x",
      call = caller_env()
    )
  }
)

setMethod(
  "bioc_recycle",
  signature = list(x = class_s4_vec),
  def = function(
    x,
    size,
    ...,
    x_arg = "",
    call = caller_env()
  ) {
    if (!is.numeric(size)) abort("argument `size` should be integer-ish")
    if (length(size) != 1L) abort("argument `size` should be length 1L")
    vec_len <- bioc_size(x)
    vec_len |>
      match(c(1L, size), nomatch = 3L) |>
      switch(
        `1` = bioc_slice(x, vctrs::vec_rep(1L, size)),
        `2` = x,
        abort(
          glue::glue("Can't recycle inpute of size {vec_len} to size {size}."),
          call = caller_env()
        )
      )
  }
)
