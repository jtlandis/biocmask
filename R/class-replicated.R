setClass(
  "Replicated",
  contains = c("VIRTUAL", "Vector"),
  slots = c("data" = "ANY", times = "integer")
)

Replicated <- function(data, times, each = FALSE) {
  times <- vctrs::vec_cast(times, integer(1L))
  vctrs::vec_assert(times, size = 1L)
  methods::new(
    if (each) "ReplicatedEach" else "ReplicatedTimes",
    data = data,
    times = times
  )
}

setClass(
  "ReplicatedTimes",
  contains = c("Replicated")
)

setClass(
  "ReplicatedEach",
  contains = c("Replicated")
)

setMethod("length", "Replicated", function(x) bioc_size(x@data) * x@times)
setMethod("show", "Replicated", function(object) {
  cat(sprintf(
    "<%s> of length %d (times = %d)\n",
    class(object)[1],
    length(object),
    object@times
  ))
  show(object@data)
})

S7::method(bioc_rep, getClass("ReplicatedTimes")) <- function(x, times, ...) {
  Replicated(x@data, times = x@times * times)
}

S7::method(bioc_rep, getClass("ReplicatedEach")) <- function(x, times, ...) {
  Replicated(x, times = times)
}

S7::method(bioc_rep_each, getClass("ReplicatedTimes")) <- function(x, times, ...) {
  Replicated(x, times = times, each = TRUE)
}

S7::method(bioc_rep_each, getClass("ReplicatedEach")) <- function(x, times, ...) {
  Replicated(x@data, times = x@times * times, each = TRUE)
}

setMethod("as.vector", "ReplicatedTimes", function(x, mode = "any") {
  data <- x@data
  if (methods::is(data, "Replicated")) {
    data <- as.vector(data)
  }
  bioc_rep(data, times = x@times)
})
setMethod("as.vector", "ReplicatedEach", function(x, mode = "any") {
  data <- x@data
  if (methods::is(data, "Replicated")) {
    data <- as.vector(data)
  }
  bioc_rep_each(data, times = x@times)
})

#' @importFrom S4Vectors showAsCell
setMethod(
  S4Vectors::showAsCell, "Replicated",
  function(object) S4Vectors::showAsCell(as.vector(object))
)

setGeneric(
  "unreplicate",
  signature = "x",
  def = function(x, recursive = FALSE) standardGeneric("unreplicate")
)

setMethod(
  "unreplicate",
  "ANY",
  definition = function(x, recursive = FALSE) x
)

setMethod(
  "unreplicate",
  "Replicated",
  function(x, recursive = FALSE) {
    if (!recursive) {
      return(x@data)
    }
    while (methods::is(x, "Replciated")) {
      x <- x@data
    }
    x
  }
)

setMethod(
  S4Vectors::extractROWS,
  c("Replicated"),
  function(x, i) {
    Replicated(x[i], 1L)
  }
)

setMethod(
  "[",
  "ReplicatedEach",
  function(x, i, ...) {
    i <- S4Vectors::normalizeSingleBracketSubscript(i, x)
    bioc_slice(x@data, i = ((i - 1L) %/% bioc_size(x@data)) + 1L)
  }
)

setMethod(
  "[",
  "ReplicatedTimes",
  function(x, i, ...) {
    i <- S4Vectors::normalizeSingleBracketSubscript(i, x)
    bioc_slice(x@data, i = ((i - 1L) %% bioc_size(x@data)) + 1L)
  }
)

test <- function() {
  browser()
  ..r[1]
}
