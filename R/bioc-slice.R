#' @title Get observations of a vector
#' @name bioc_slice
#' @description
#' This extends `vctrs::vec_slice` to `S4Vectors::Vector` class by wrapping
#' `vec_slice` with an S4 standard generic named `bioc_slice`. Atomic vectors
#' and other base S3 classes
#'  (list, data.frame, factor, Dat, POSIXct) will dispatch to the
#' `vctrs::vec_slice` method as normal. Dispatch support on the
#' `S4Vectors::Vector` and `S4Vectors::DataFrame` classes provides a unified
#' framework for working with base R vectors and `S4Vectors`.
#'
#' ### `S4Vectors::Vector` Implementation
#'
#' This method will naively call the `[` method for any S4 class that inherits
#' from the `S4Vectors::Vector` class. This may not be a very efficient way to
#' slice up an S4 class, but will work.
#'
#' With this implementation, the `x@mcol` data is expected to be retained after
#' a call to `biocmask::bioc_slice(x, i)`.
#'
#' ### `S4Vectors::DataFrame` Implementation
#'
#' The `DataFrame` implementation works similar to how `vctrs::bioc_slice` works
#' on a `data.frame` object. What is being sliced is the rows of `x@listData`.
#' To maintain the size stability of the `DataFrame` object, we change `@nrows`
#' to the appropriate value, and perform a recursive call if `@elementMetadata`
#' is not `NULL`.
#'
#' ## Performance
#'
#' Depending on the size and complexity of your S4 Vector object, you may find
#' the standard subset operation is extremely slow. For example, consider a
#' `SummarizedExperiment` whose rowData contains a `CompressedGRangesList`
#' object assigned to the name "exons" and whose length is 250,000 and
#' underlying `@unlistData` is length 1,600,000. Performing a by `.features`
#' grouping operation and attempting to evaluate the `exons` within the row
#' context would force the `CompressedGRangesList` object to be
#' chopped element-wise.
#'
#' Note that biocmask's `bioc_slice` method is provided as an S4 generic so
#' that other developers may implement possibly faster methods for slicing
#' besides the default `[`. A solution for slicing a `CompressedGRangesList`
#' or `GRanges` object may eventually be provided by a supplemental package.
#'
#' @inheritParams vctrs::vec_slice
#' @return a new S3 or S4 vector subsetted by `i`
#' @examples
#' bioc_slice(1:10, i = 5)
#' bioc_slice(S4Vectors::Rle(rep(1:3, each = 3)), i = 5)
#'
#' @export
bioc_slice <- S7::new_generic(
  "bioc_slice",
  dispatch_args = "x",
  function(x, i, ...) S7_dispatch()
)

S7::method(
  bioc_slice,
  signature = class_vec
) <- function(x, i, ...) x[i]

S7::method(
  bioc_slice,
  signature = class_vctrs_vec
) <- function(x, i, ...) vctrs::vec_slice(x, i)

S7::method(
  bioc_slice,
  signature = class_s4_vec
) <- function(x, i, ...) {
  x[i]
}


slice_DF <- function(x, i, ...) {
  data <- lapply(x@listData, bioc_slice, i = i)
  nrows <- length(i)
  if (!is.null(rnames <- rownames(x))) {
    rnames <- bioc_slice(rnames, i)
  }

  new_DF(
    .data = data,
    nrows = nrows,
    rownames = rnames
  )
}

S7::method(
  bioc_slice,
  signature = class_DF
) <- slice_DF

S7::method(
  bioc_slice,
  signature = class_df
) <- function(x, i, ...) {
  vctrs::vec_slice(x, i)
}

# method(bioc_slice, getClass("CompressedGRangesList")) <- function(x, i, ...) {
#  st <- IRanges::start(x@partitioning)
#  en <- IRanges::end(x@partitioning)
#  w <- IRanges::width(x@partitioning)[i]
#  seqs <- purrr::map2(st[i], en[i], `:`)
#
#  gr <- IRanges::extractROWS(x@unlistData, vctrs::vec_c(rlang::splice(seqs)))
#
#  cgr <- as(gr, "CompressedGRangesList")
#  S4Vectors::new2(
#    "CompressedGRangesList",
#    unlistData = gr,
#    partitioning = IRanges::PartitioningByEnd(
#      x = cumsum(w),
#      names = names(x@partitioning)[i]
#    ),
#    elementType = "GRanges",
#    elementMetadata = IRanges::extractROWS(x@elementMetadata, i),
#    metadata = x@metadata
#  )
# }

# method(bioc_slice, class_vec_phantom) <- function(x, i, ...) {
#  attr(x, "phantomData") <- bioc_slice(attr(x, "phantomData"), i)
#  x
# }

# Unfortunately, there is a massive performance hit in attempting to construct
# 250,000 `GRanges`. Unless you do not mind waiting over an hour for each
# `dplyr` verb in which `exons` gets evaluated, consider `biocmask_s4_proxy_vec()`.
# This attempts to reconstruct certain standard `S4Vectors::Vectors` as
# standard vectors or tibbles. The equivalent `exons` object would require
# much more memory use, but at the advantage of only taking several seconds to
# construct.When you are done, you can attempt to restore the original S4
# Vector with `biocmask_restore_s4_proxy()`.
#
# `biocmask_s4_proxy_vec()` is faster to work with because there are less
# checks on the object validity and all `@elementMetadata` and `@metadata` are
# dropped from the objects.
