#' create a prototype of an object
#' @name bioc_ptype
#' @description
#' get the zero size prototype of an object
#' @return an object of size 0
#' @export
setGeneric(
  "bioc_ptype",
  signature = "x",
  def = function(x, ...) {
    standardGeneric("bioc_ptype")
  }
)

setMethod(
  "bioc_ptype",
  signature = list(x = class_vec),
  def = function(
      x,
      ...) {
    vctrs::vec_ptype(
      x = x,
      ...,
      x_arg = "x",
      call = caller_env()
    )
  }
)

setMethod(
  "bioc_ptype",
  signature = list(x = class_vctrs_vec),
  def = function(
      x,
      ...) {
    vctrs::vec_ptype(
      x = x,
      ...,
      x_arg = "x",
      call = caller_env()
    )
  }
)

setMethod(
  "bioc_ptype",
  signature = list(x = class_df),
  def = function(
      x,
      ...) {
    vctrs::vec_ptype(
      x = x,
      ...,
      x_arg = "x",
      call = caller_env()
    )
  }
)

setMethod(
  "bioc_ptype",
  signature = list(x = class_s4_vec),
  def = function(
      x,
      ...) {
    x[0]
  }
)

setMethod(
  "bioc_ptype",
  signature = list(x = class_s4_vec),
  def = function(
      x,
      ...) {
    x[0]
  }
)

setMethod(
  "bioc_ptype",
  signature = list(x = class_DF),
  def = function(x,
                 ...) {
    slice_DF(x, 0L, ...)
  }
)
