#' create a prototype of an object
#' @name bioc_ptype
#' @description
#' get the zero size prototype of an object
#' @return an object of size 0
#' @export
bioc_ptype <- S7::new_generic(
  "bioc_ptype",
  dispatch_args = "x",
  function(x, ...) {
    S7_dispatch()
  }
)

S7::method(
  bioc_ptype,
  signature = class_vec
) <- function(x,
              ...) {
  vctrs::vec_ptype(
    x = x,
    ...,
    x_arg = "x",
    call = caller_env()
  )
}


S7::method(
  bioc_ptype,
  signature = class_vctrs_vec
) <- function(x,
              ...) {
  vctrs::vec_ptype(
    x = x,
    ...,
    x_arg = "x",
    call = caller_env()
  )
}


S7::method(
  bioc_ptype,
  signature = class_df
) <- function(x,
              ...) {
  vctrs::vec_ptype(
    x = x,
    ...,
    x_arg = "x",
    call = caller_env()
  )
}


S7::method(
  bioc_ptype,
  signature = class_s4_vec
) <- function(x,
              ...) {
  x[0]
}


S7::method(
  bioc_ptype,
  signature = class_DF
) <- function(x,
              ...) {
  slice_DF(x, 0L, ...)
}
