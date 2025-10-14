#' bioc_size
#' @export
bioc_size <- S7::new_generic(
  "bioc_size",
  dispatch_args = "x",
  function(x, ...) {
    S7_dispatch()
  }
)

S7::method(
  bioc_size,
  signature = class_vec
) <- function(x, ...) {
  length(x)
}

S7::method(
  bioc_size,
  signature = class_s4_vec
) <- function(x, ...) {
  length(x)
}

S7::method(
  bioc_size,
  signature = class_vctrs_vec
) <- function(x, ...) {
  vctrs::vec_size(x)
}

S7::method(
  bioc_size,
  signature = class_df
) <- function(x, ...) {
  vctrs::vec_size(x)
}

S7::method(
  bioc_size,
  signature = class_DF
) <- function(x, ...) {
  nrow(x)
}

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
