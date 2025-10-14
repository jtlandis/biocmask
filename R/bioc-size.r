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

bioc_size_common_list <- function(dots) {
  size <- 1L
  j <- 0L
  iseq <- seq_len(length(dots))
  for (i in iseq) {
    dot <- .subset2(dots, i)
    dot_size <- bioc_size(dot)
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

#' @export
bioc_size_common <- function(...) {
  bioc_size_common_list(rlang::list2(...))
}
