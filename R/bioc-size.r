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

#' @export
bioc_size_common <- function(...) {
  size <- 1L
  j <- 0L
  for (i in seq_len(...length())) {
    dot_size <- bioc_size(...elt(i))
    if (dot_size != 1L) {
      if (size == 1L) {
        size <- dot_size
        j <- i
      } else if (size != dot_size) {
        stop(
          sprintf(
            "Can't recycle `..%i` (size %i) to match `..%i` (size %i)",
            j, size, i, dot_size
          )
        )
      }
    }
  }
  size
}
