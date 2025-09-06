#' @title classes for vctrs and S4 Vectors
#' @name vectors
#' @description
#' A set of classes definitions and Class unions that help establish S4 method
#' dispatch.
#' These classes were made to re-export several `vctrs` functions such that
#' internals for `biocmask` were consistent with room for optimization.
#' @seealso [bioc_rep()],[bioc_recycle()],[bioc_slice()]
#' @return S4 class union or base class
#' @export
class_vec <- methods::getClass("vector")

methods::setOldClass("vctrs_vctr")

#' @rdname vectors
#' @export
class_vctrs_vec <- getClass("vctrs_vctr")

#' @rdname vectors
#' @export
class_s4_vec <- methods::getClass("Vector", where = "S4Vectors")

#' @rdname vectors
#' @export
class_DF <- methods::getClass("DataFrame", where = "S4Vectors")

#' @rdname vectors
#' @export
class_df <- methods::getClass("data.frame")

#' @rdname vectors
#' @export
class_GR <- methods::getClass("GRanges", where = "GenomicRanges")
