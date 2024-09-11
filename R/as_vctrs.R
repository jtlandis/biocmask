
##################
## EXPERIMENTAL ##
##################

as_s4_proxy <- function(x) {
  UseMethod("as_s4_proxy")
}

s4_proxy_restore <- function(x) {
  UseMethod("s4_proxy_restore")
}

new_s4_proxy <- function(.data, .class) {
  # attr(.data, "s4_proxy") <- original
  class(.data) <- c("s4_proxy", sprintf("%s_proxy", .class), class(.data))
  .data
}

`[.s4_proxy` <- function(x, i, j) {
  cl <- class(x)
  out <- NextMethod()
  class(out) <- cl
  out
}



as_s4_proxy.Rle <- function(x) {
  .data <- rep(x@values, times = x@lengths)
  new_s4_proxy(.data, class(x))
}

zap_proxy <- function(x) {
  class(x) <- grep("_proxy$", class(x), invert = TRUE, value = TRUE)
  attr(x, "s4_proxy") <- NULL
  x
}

s4_proxy_restore.Rle_proxy <- function(x) {
  S4Vectors::Rle(values = zap_proxy(x))  
}


as_s4_proxy.IRanges <- function(x) {
  new_s4_proxy(
    tibble(start = x@start,
         width = x@width,
         NAMES = x@NAMES), class(x))
}

s4_proxy_restore.IRanges_proxy <- function(x) {
  IRanges::IRanges(
    start = x$start,
    width = x$width,
    names = x$NAMES
  )
}



as_s4_proxy.GRanges <- function(x) {
  
  new_s4_proxy(
    tibble(
      seqnames = zap_proxy(as_s4_proxy(x@seqnames)),
      ranges = zap_proxy(as_s4_proxy(x@ranges)),
      strand = zap_proxy(as_s4_proxy(x@strand))
    ), 
    class(x)
  )
  
}

s4_proxy_restore.GRanges_proxy <- function(x) {
  GenomicRanges::GRanges(
    seqnames = S4Vectors::Rle(x$seqnames),
    ranges = IRanges::IRanges(
      start = x$ranges$start,
      width = x$ranges$width,
      names = x$ranges$NAMES
    ),
    strand = S4Vectors::Rle(x$strand)
  )
}

as_s4_proxy.CompressedGRangesList <- function(x) {
  
  indices <- purrr::map2(IRanges::start(x@partitioning),
                         IRanges::end(x@partitioning),
                         `:`)
  proxy <- as_s4_proxy.GRanges(x@unlistData)
  new_list_of(
    vctrs::vec_chop(proxy, indices),
    ptype = tibble::tibble(),
    class = c("s4_proxy", sprintf("%s_proxy", class(x))),
    partitioning = x@partitioning,
    elementMetadata = x@elementMetadata
  )
}

`[.CompressedGRangesList_proxy` <- function(x, i) {
  p <- attr(x, "partitioning")
  out <- NextMethod()
  attr(out, "partitioning") <- IRanges::PartitioningByEnd(
    x = cumsum(IRanges::width(p)[i]), names = names(p)[i]
  )
  attr(out, "elementMetadata") <- attr(out, "elementMetadata")[i,]
  out
}


s4_proxy_restore.CompressedGRangesList_proxy <- function(x) {
  # browser()
  
  p <- attr(x, "partitioning")
  
  flattened <- lapply(1:3, function(i) {
    lapply(x, .subset2, i)
  })
  
  # comparable to `c()`, but uses less memory
  seqnames <- vec_c(rlang::splice(flattened[[1L]]))
  ranges <- dplyr::bind_rows(flattened[[2L]])
  strand <- vec_c(rlang::splice(flattened[[3L]]))
  
  gr <- GenomicRanges::GRanges(
    seqnames = S4Vectors::Rle(seqnames),
    ranges = IRanges::IRanges(
      start = ranges$start,
      width = ranges$width,
      names = ranges$NAMES
    ),
    strand = S4Vectors::Rle(strand)
  )
  
  out <- methods::as(gr, "CompressedGRangesList")
  out@partitioning  <- p
  out@elementMetadata <- attr(x, "elementMetadata")
  out
  
}
