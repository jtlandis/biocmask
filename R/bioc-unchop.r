#' unchop a list-like object into some prototype
#' @param x a list
#' @param ptype prototype to unchop into
#' @param ... unused
#' @param indices optional list of integer vectors giving locations to splice
#' elements x into the final vector.
#' @export
bioc_unchop <- function(x, ptype, ..., indices = NULL) {
  vctrs::vec_assert(x, ptype = list())
  bioc_unchop_ptype(ptype = ptype, x = x, ..., indices = indices)
}

#' @rdname bioc_unchop
#' @export
bioc_unchop_ptype <- S7::new_generic(
  "bioc_unchop_ptype",
  dispatch_args = "ptype",
  function(ptype, x, ..., indices = NULL) S7_dispatch()
)

S7::method(
  bioc_unchop_ptype,
  signature = class_vec
) <- function(ptype, x, ..., indices = NULL) {
  vctrs::list_unchop(x, indices = indices, ptype = ptype)
}

S7::method(
  bioc_unchop_ptype,
  signature = S7::class_any,
) <- function(ptype, x, ..., indices = NULL) {
  vctrs::list_unchop(x, indices = indices, ptype = NULL)
}

S7::method(
  bioc_unchop_ptype,
  signature = class_vctrs_vec
) <- function(ptype, x, ..., indices = NULL) {
  vctrs::list_unchop(x, indices = indices, ptype = ptype)
}

S7::method(
  bioc_unchop_ptype,
  signature = class_s4_vec,
) <- function(ptype, x, ..., indices = NULL) {
  merged <- do.call("c", x)
  if (!is.null(indices)) {
    # fun <- selectMethod("bioc_size", c(x = class(x[[1]])))
    indices <- vctrs::list_unchop(indices)
    merged <- bioc_slice(merged, order(indices))
  }
  merged
}

S7::method(
  bioc_unchop_ptype,
  signature = NULL
) <- function(ptype, x, ..., indices = NULL) {
  ptype <- bioc_ptype_common_list(x, NULL)
  if (is.null(ptype)) {
    rlang::abort("could not resolve ptype")
  }
  bioc_unchop_ptype(
    x,
    ptype = ptype,
    indices = indices
  )
}
