#' @export
setGeneric(
  "bioc_unchop",
  signature = c("x", "ptype"),
  def = function(x, ptype, ..., indices = NULL) standardGeneric("bioc_unchop")
)

setMethod(
  "bioc_unchop",
  signature = list(x = "list", ptype = class_vec),
  def = function(x, ptype, ..., indices = NULL) {
    vctrs::list_unchop(x, indices = indices, ptype = ptype)
  }
)

setMethod(
  "bioc_unchop",
  signature = list(x = "list", ptype = "ANY"),
  def = function(x, ptype, ..., indices = NULL) {
    vctrs::list_unchop(x, indices = indices, ptype = NULL)
  }
)

setMethod(
  "bioc_unchop",
  signature = list(x = "list", ptype = class_vctrs_vec),
  def = function(x, ptype, ..., indices = NULL) {
    vctrs::list_unchop(x, indices = indices, ptype = ptype)
  }
)

setMethod(
  "bioc_unchop",
  signature = list(x = "list", ptype = class_s4_vec),
  def = function(x, ptype, ..., indices = NULL) {
    merged <- do.call("c", x)
    if (!is.null(indices)) {
      fun <- selectMethod("bioc_size", c(x = class(x[[1]])))
      indices <- vctrs::list_unchop(indices)
      merged <- bioc_slice(merged, order(indices))
    }
    merged
  }
)
