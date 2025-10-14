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
bioc_rep <- S7::new_generic(
  "bioc_rep",
  dispatch_args = "x",
  def = function(x, times, ...) S7_dispatch()
)

S7::method(
  bioc_rep,
  signature = list(x = class_vec)
) <- function(x, times, ...) {
  vctrs::vec_rep(
    x = x,
    times = times,
    ...,
    error_call = caller_env(),
    x_arg = "x",
    times_arg = "times"
  )
}


S7::method(
  bioc_rep,
  signature = list(x = class_vctrs_vec)
) <- function(x, times, ...) {
  vctrs::vec_rep(
    x = x,
    times = times,
    ...,
    error_call = caller_env(),
    x_arg = "x",
    times_arg = "times"
  )
}

S7::method(
  bioc_rep,
  signature = list(x = class_df)
) <- function(x, times, ...) {
  vctrs::vec_rep(
    x = x,
    times = times,
    ...,
    error_call = caller_env(),
    x_arg = "x",
    times_arg = "times"
  )
}

S7::method(
  bioc_rep,
  signature = list(x = class_s4_vec)
) <- function(
    x,
    times,
    ...) {
  times <- vctrs::vec_cast(times, integer(1), x_arg = "times")
  if (length(times) != 1L) abort("`times` must be a single number")
  rep(x, times)
}

#' @rdname vctrs-bioc_rep
#' @export
bioc_rep_each <- S7::new_generic(
  "bioc_rep_each",
  dispatch_args = "x",
  def = function(x, times, ...) S7_dispatch()
)

S7::method(
  bioc_rep_each,
  signature = list(x = class_vec)
) <- function(
    x,
    times,
    ...) {
  vctrs::vec_rep_each(
    x = x,
    times = times,
    ...,
    x_arg = "x",
    times_arg = "times",
    error_call = caller_env()
  )
}

S7::method(
  bioc_rep_each,
  signature = list(x = class_vctrs_vec)
) <- function(
    x,
    times,
    ...) {
  vctrs::vec_rep_each(
    x = x,
    times = times,
    ...,
    x_arg = "x",
    times_arg = "times",
    error_call = caller_env()
  )
}

S7::method(
  bioc_rep_each,
  signature = list(x = class_s4_vec)
) <- function(
    x,
    times,
    ...) {
  times <- vctrs::vec_cast(times, integer(1), x_arg = "times")
  if (length(times) != 1L) abort("`times` must be a single number")
  rep(x, each = times)
}
