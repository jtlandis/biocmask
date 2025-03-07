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
bioc_recycle <- new_generic(
  "bioc_recycle",
  dispatch_args = "x",
  function(x, size, ..., x_arg = "", call = caller_env()) {
    S7_dispatch()
  }
)

method(bioc_recycle, class_vctrs) <- function(
  x,
  size,
  ...,
  x_arg = "",
  call = caller_env()
) {
  vctrs::vec_recycle(x = x, size = size, ..., x_arg = x_arg, call = call)
}

method(bioc_recycle, class_s4_vctrs) <- function(
  x,
  size,
  ...,
  x_arg = "",
  call = caller_env()
) {
  if (length(size) != 1L) abort("argument `size` should be length 1L")
  if (!is.numeric(size)) abort("argument `size` should be integer-ish")
  vec_len <- length(x)
  vec_len |>
    match(c(1L, size), nomatch = 0L) |>
    as.character() |>
    switch(
      `1` = bioc_slice(x, vctrs::vec_rep(1L, size)),
      `2` = x,
      abort(
        glue::glue("Can't recycle inpute of size {vec_len} to size {size}."),
        call = call
      )
    )
}
