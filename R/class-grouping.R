#' @include utils.R

setClass(
  "IndexGrouping",
  contains = c("Vector"),
  slots = c("indices" = "list")
)

setMethod(
  "as.list",
  "IndexGrouping",
  function(x, ...) x@indices
)

setAs(
  "IndexGrouping",
  "List",
  function(from, to) {
    S4Class <- if (require("IRanges", quietly = TRUE)) {
      "SimpleIntegerList"
    } else {
      "SimpleList"
    }
    new(
      S4Class,
      listData = from@indices,
      elementType = "integer",
      elementMetadata = S4Vectors::mcols(from),
      metadata = S4Vectors::metadata(from)
    )
  }
)

#' @export
IndexGrouping <- function(..., indices = list()) {
  dots <- Filter(function(x) !is.null(x), list(...))
  mcols <- if (length(dots) == 0) {
    S4Vectors::make_zero_col_DFrame(length(indices))
  } else {
    S4Vectors::DataFrame(dots)
  }
  grp <- new(
    "IndexGrouping",
    indices = indices,
    elementMetadata = mcols
  )
  grp
}

#' @export
as_index_grouping <- function(x) {
  x <- bioc_group_loc(x)
  mcols <- as(x[["key"]], "DataFrame")
  rownames(mcols) <- NULL
  new(
    "IndexGrouping",
    indices = x[["loc"]],
    elementMetadata = mcols
  )
}

#' @export
setAs(
  "ANY",
  "IndexGrouping",
  function(from) as_index_grouping(from)
)


#' @export
setMethod(
  "parallel_slot_names",
  "IndexGrouping",
  function(x) {
    c("indices", "elementMetadata")
  }
)

setMethod(
  "show",
  "IndexGrouping",
  function(object) {
    mcols <- S4Vectors::mcols(object)
    cat(sprintf(
      "%s with %i groups and %i keys\n",
      class(object)[[1]],
      NROW(object),
      length(mcols)
    ))
    df <- S4Vectors::DataFrame(indices = S4Vectors::I(object@indices), mcols)
    out <- utils::capture.output(show(df))[-1L]
    cat(out, sep = "\n")
  }
)

setMethod(
  "$",
  "IndexGrouping",
  function(x, name) S4Vectors::mcols(x, use.names = FALSE)[[name]]
)

.DollarNames.IndexGrouping <- function(x, pattern = "") {
  grep(pattern, names(S4Vectors::mcols(x, use.names = FALSE)), value = TRUE)
}

# setMethod(
#   S4Vectors::extractROWS,
#   c("IndexGrouping", "ANY"),
#   function(x, i) {
#     x <- callNextMethod()
#     slot(x, "indices", check = FALSE) <- S4Vectors::extractROWS(
#       slot(x, "indices"), i
#     )
#     x
#   }
# )
