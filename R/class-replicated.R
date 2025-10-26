setClass(
  "Replicated",
  contains = c("VIRTUAL", "Vector"),
  slots = c("data" = "ANY", times = "integer")
)

new_Replicated <- function(data, times, each = FALSE) {
  methods::new(
    if (each) "ReplicatedEach" else "ReplicatedTimes",
    data = data,
    times = times
  )
}

Replicated <- function(data, times, each = FALSE, compressed = TRUE) {
  times <- vctrs::vec_cast(times, integer(1L))
  vctrs::vec_assert(times, size = 1L)
  vctrs::vec_assert(each, ptype = logical(), size = 1L)
  vctrs::vec_assert(compressed, ptype = logical(), size = 1L)
  if (compressed && methods::is(data, "Replicated")) {
    # see if we can simplify the data
    target <- if (each) "ReplicatedEach" else "ReplicatedTimes"
    if (methods::is(data, target)) {
      data@times <- data@times * times
      return(data)
    } else if (methods::is(data@data, target)) {
      old_data <- data@data
      data@data <- new_Replicated(
        old_data@data,
        old_data@times * times,
        each = each
      )
      return(data)
    }
  }
  new_Replicated(data, times, each = each)
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
  new_Replicated(x@data, times = x@times * times)
}

S7::method(bioc_rep, getClass("ReplicatedEach")) <- function(x, times, ...) {
  new_Replicated(x, times = times)
}

S7::method(bioc_rep_each, getClass("ReplicatedTimes")) <- function(x, times, ...) {
  new_Replicated(x, times = times, each = TRUE)
}

S7::method(bioc_rep_each, getClass("ReplicatedEach")) <- function(x, times, ...) {
  new_Replicated(x@data, times = x@times * times, each = TRUE)
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
    while (methods::is(x, "Replicated")) {
      x <- x@data
    }
    x
  }
)

setGeneric(
  "unreplicate<-",
  signature = "x",
  def = function(x, recursive = FALSE, value) standardGeneric("unreplicate<-")
)

setMethod(
  `unreplicate<-`,
  signature = "Replicated",
  def = function(x, recursive = FALSE, value) {
    data <- unreplicate(x, recursive = recursive)
    if (length(data) != length(value)) {
      rlang::abort("Replacement value must have the same length as the unreplicated object.")
    }
    if (recursive && methods::is(x@data, "Replicated")) {
      unreplicate(x@data, recursive = recursive) <- value
    } else {
      x@data <- value
    }
    x
  }
)

setMethod(
  `unreplicate<-`,
  signature = "ANY",
  def = function(x, recursive = FALSE, value) {
    x <- value
    x
  }
)

setMethod(
  S4Vectors::extractROWS,
  c("Replicated"),
  function(x, i) {
    new_Replicated(x[i], 1L)
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
