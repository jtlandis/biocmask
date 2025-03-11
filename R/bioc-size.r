#' bioc_size
#' @export
setGeneric(
  "bioc_size",
  signature = "x",
  def = function(x, ...) {
    standardGeneric("bioc_size")
  }
)

setMethod(
  "bioc_size",
  signature = list(x = class_vec),
  def = function(x, ...) {
    length(x)
  }
)

setMethod(
  "bioc_size",
  signature = list(x = class_s4_vec),
  def = function(x, ...) {
    length(x)
  }
)

setMethod(
  "bioc_size",
  signature = list(x = class_vctrs_vec),
  def = function(x, ...) {
    vctrs::vec_size(x)
  }
)

setMethod(
  "bioc_size",
  signature = list(x = class_df),
  def = function(x, ...) {
    vctrs::vec_size(x)
  }
)

setMethod(
  "bioc_size",
  signature = list(x = class_DF),
  def = function(x, ...) {
    nrow(x)
  }
)
