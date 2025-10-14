#' @title classes for vctrs and S4 Vectors
#' @name vectors
#' @description
#' A set of classes definitions and Class unions that help establish S4 method
#' dispatch.
#' These classes were made to re-export several `vctrs` functions such that
#' internals for `biocmask` were consistent with room for optimization.
#' @return S4 class union or base class
#' @export
class_vec <- S7::new_union(S7::class_atomic, S7::class_list, S7::class_factor)

# using S7::class_vector would include S7::class_expression, which we don't
# want to define methods for.


#' @rdname vectors
#' @export
class_vctrs_vec <- S7::new_S3_class("vctrs_vctr")

#' @rdname vectors
#' @export
class_s4_vec <- methods::getClass("Vector", where = "S4Vectors")

#' @rdname vectors
#' @export
class_DF <- methods::getClass("DataFrame", where = "S4Vectors")

#' @rdname vectors
#' @export
class_df <- S7::class_data.frame
