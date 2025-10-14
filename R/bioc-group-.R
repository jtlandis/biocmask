#' @title Recycle a vector
#' @name bioc_group_id
#' @description
#' A alternative to [`vctrs::vec_group_id`][vctrs::vec_group_id] and
#' [`vctrs::vec_group_loc`][vctrs::vec_group_loc] as
#' an S4 generic function to allow `S4Vectors`.
#' @inheritParams vctrs::vec_group_id
#' @return either a tibble or DataFrame object
#' @examples
#'
#' data <- DataFrame(
#'   letter = sample(letters, 500, TRUE),
#'   LETTER = sample(letters, 500, TRUE)
#' )
#'
#' bioc_group_id(data)
#' bioc_group_loc(data)
#'
#' @export
bioc_group_id <- S7::new_generic(
  "bioc_group_id",
  dispatch_args = "x",
  function(x, ...) {
    S7_dispatch()
  }
)

S7::method(
  bioc_group_id,
  signature = list(x = class_vec)
) <- function(x, ...) {
  vctrs::vec_group_id(x = x)
}


S7::method(
  bioc_group_id,
  signature = list(x = class_vctrs_vec)
) <- function(x, ...) {
  vctrs::vec_group_id(x = x)
}


S7::method(
  bioc_group_id,
  signature = class_df
) <- function(x, ...) {
  vctrs::vec_group_id(x = x)
}


S7::method(
  bioc_group_id,
  signature = class_s4_vec
) <- function(x, ...) {
  as(BiocGenerics::match(x, BiocGenerics::unique(x)), "integer")
}


S7::method(
  bioc_group_id,
  signature = class_DF
) <- function(x, ...) {
  lapply(x@listData, bioc_group_id) |>
    as.data.frame() |>
    vctrs::vec_group_id()
}


## -----

#' @rdname bioc_group_id
#' @export
setGeneric(
  "bioc_group_loc",
  signature = "x"
) <- function(x, ...) {
  standardGeneric("bioc_group_loc")
}


S7::method(
  bioc_group_loc,
  signature = class_vec
) <- function(x, ...) {
  vctrs::vec_group_loc(x = x)
}


S7::method(
  bioc_group_loc,
  signature = class_vctrs_vec
) <- function(x, ...) {
  vctrs::vec_group_loc(x = x)
}


S7::method(
  bioc_group_loc,
  signature = class_df
) <- function(x, ...) {
  vctrs::vec_group_loc(x = x)
}


S7::method(
  bioc_group_loc,
  signature = class_s4_vec
) <- function(x, ...) {
  # bioc_group_id is based on the object `x` and NOT
  # the mcols data. So for any arbitrary class `x` may
  # be, we do not consider the elementMetadata
  #
  # `Vector` extends Annotated, thus `mcols(x)` exists
  mcols(x) <- NULL
  id <- bioc_group_id(x)
  loc <- bioc_size(x) |>
    seq_len() |>
    split(f = id) |>
    unname()

  as_DF(
    list(
      key = bioc_slice(x, vapply(loc, `[`, 1L, 1L)),
      loc = loc
    )
  )
}


S7::method(
  bioc_group_loc,
  signature = class_DF
) <- function(x, ...) {
  out <- lapply(x@listData, bioc_group_id) |>
    as.data.frame() |>
    vctrs::vec_group_loc()
  out <- as_DF(as.list(out))
  out$key <- bioc_slice(x, i = vapply(out$loc, `[`, 1L, 1L))
  out
}


#' create groups
#' @export
create_groups <- function(.data, .rename = ".indices") {
  # browser()
  # check if length > 0
  if (is_empty(.data)) {
    return(NULL)
  }
  # check first index has length > 0
  # assumes all others have similar length (probably not always true)
  if (length(.data[[1]]) == 0) {
    .data <- tibble::as_tibble(.data)
    .data[[.rename]] <- list()
    .data[[sprintf("%s_group_id", .rename)]] <- integer()
    return(.data)
  }
  grouped <- bioc_group_loc(.data)
  .data <- c(
    as.list(grouped$key),
    setNames(list(grouped$loc), .rename),
    setNames(list(seq_len(length(grouped$loc))), sprintf("%s_group_id", .rename))
  )
  # .data <- as(grouped$key, "DataFrame")
  # .data[[.rename]] <- grouped$loc
  # .data[[sprintf("%s_group_id", .rename)]] <- seq_len(length(grouped$loc))
  as_DF(.data)
}
