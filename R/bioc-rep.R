#' @title replicate a vector
#' @name vctrs-bioc_rep
#' @description
#' A alternative to [`vctrs::vec_rep`][vctrs::vec_rep] and
#' [`vctrs::vec_rep_each`][vctrs::vec_rep_each] as an S4 generic
#' function to allow `S4Vectors`.
#' @inheritParams vctrs::vec_rep
#' @return a new S3 or S4 vector replicated by specified times
#' @examples
#' bioc_rep(1:2, times = 5)
#' bioc_rep(S4Vectors::Rle(1:2), times = 5)
#'
#' bioc_rep_each(1:2, times = 5)
#' bioc_rep_each(S4Vectors::Rle(1:2), times = 5)
#' @export
setGeneric(
  "bioc_rep",
  signature = "x",
  def = function(x, times, ...) standardGeneric("bioc_rep")
)

setMethod(
  "bioc_rep",
  signature = list(x = class_vec),
  function(x, times, ...) {
    vctrs::vec_rep(
      x = x,
      times = times,
      ...,
      error_call = caller_env(),
      x_arg = "x",
      times_arg = "times"
    )
  }
)

setMethod(
  "bioc_rep",
  signature = list(x = class_vctrs_vec),
  function(x, times, ...) {
    vctrs::vec_rep(
      x = x,
      times = times,
      ...,
      error_call = caller_env(),
      x_arg = "x",
      times_arg = "times"
    )
  }
)

setMethod(
  "bioc_rep",
  signature = list(x = class_df),
  function(x, times, ...) {
    vctrs::vec_rep(
      x = x,
      times = times,
      ...,
      error_call = caller_env(),
      x_arg = "x",
      times_arg = "times"
    )
  }
)

setMethod(
  "bioc_rep",
  signature = list(x = class_s4_vec),
  function(
    x,
    times,
    ...
  ) {
    times <- vctrs::vec_cast(times, integer(1), x_arg = "times")
    if (length(times) != 1L) abort("`times` must be a single number")
    rep(x, times)
  }
)

#' @rdname vctrs-bioc_rep
#' @export
setGeneric(
  "bioc_rep_each",
  signature = "x",
  def = function(x, times, ...) standardGeneric("bioc_rep_each")
)

setMethod(
  "bioc_rep_each",
  signature = list(x = class_vec),
  function(
    x,
    times,
    ...
  ) {
    vctrs::vec_rep_each(
      x = x,
      times = times,
      ...,
      x_arg = "x",
      times_arg = "times",
      error_call = caller_env()
    )
  }
)

setMethod(
  "bioc_rep_each",
  signature = list(x = class_vctrs_vec),
  function(
    x,
    times,
    ...
  ) {
    vctrs::vec_rep_each(
      x = x,
      times = times,
      ...,
      x_arg = "x",
      times_arg = "times",
      error_call = caller_env()
    )
  }
)

setMethod(
  "bioc_rep_each",
  signature = list(x = class_s4_vec),
  function(
    x,
    times,
    ...
  ) {
    times <- vctrs::vec_cast(times, integer(1), x_arg = times_arg)
    if (length(length) != 1L) abort("`times` must be a single number")
    rep(x, each = times)
  }
)
