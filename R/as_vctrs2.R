


new_biocmask_s4_proxy <- function(.fields, ..., .class) {
  
  new_rcrd(
    fields = .fields,
    ...,
    proxy_for = .class,
    class = c("biocmask_s4_proxy",
              sprintf("%s_proxy", .class))
  )
  
}

elementMetadata_proxy <- function(x) {
  mcol <- x@elementMetadata
  if (is.null(mcol)) {
    tibble::new_tibble(list(),
                       nrow = length(x),
                       class = "biocmask_elementMetadata_null")
  } else {
    tibble::new_tibble(
      lapply(mcol, as_biocmask_s4_proxy),
      nrow = length(x),
      class = "biocmask_elementMetadata_proxy"
    )
  }
}

restore_elementMetadata <- function(to, from) {
  return(to)
  mcol <- from$elementMetadata
  if (inherits(mcol, "biocmask_elementMetadata_null")) {
    mcol <- NULL
  } else {
    mcol <- as(mcol, "DataFrame")
    mcol[] <- lapply(mcol, restore_biocmask_s4_proxy)
  }
  to@elementMetadata <- mcol
  to
}

as_biocmask_s4_proxy <- function(x) {
  UseMethod("as_biocmask_s4_proxy")
}


restore_biocmask_s4_proxy <- function(x) {
  UseMethod("restore_biocmask_s4_proxy")
}

# vec_proxy.biocmask_s4_proxy <- function(x) unclass(x)

as_biocmask_s4_proxy.default <- function(x) x

restore_biocmask_s4_proxy.default <- function(x) x

as_biocmask_s4_proxy.Rle <- function(x) {
  # browser()
  new_biocmask_s4_proxy(
    .fields = list(
      .data = rep(x@values, times = x@lengths)#,
      # elementMetadata = elementMetadata_proxy(x)
    ),
    .class = class(x)
  )
}

format.biocmask_s4_proxy <- function(x) {
  attr(x, "proxy_for")
}



restore_biocmask_s4_proxy.Rle_proxy <- function(x) {
  proxy <- vec_proxy(x)
  out <- S4Vectors::Rle(values = proxy$.data)
  restore_elementMetadata(out, proxy)
}


as_biocmask_s4_proxy.IRanges <- function(x) {
  new_biocmask_s4_proxy(
    .fields = list(
      start = x@start,
      width = x@width,
      NAMES = x@NAMES#,
      # elementMetadata = elementMetadata_proxy(x)
    ),
    .class = class(x)
  )
}

restore_biocmask_s4_proxy.IRanges_proxy <- function(x) {
  proxy <- vec_proxy(x)
  out <- IRanges::IRanges(
    start = proxy$start,
    width = proxy$width,
    names = proxy$NAMES
  )
  restore_elementMetadata(out, proxy)
}

as_biocmask_s4_proxy.GRanges <- function(x) {
  new_biocmask_s4_proxy(
    .fields = list(
      seqnames = as_biocmask_s4_proxy.Rle(x@seqnames),
      ranges = as_biocmask_s4_proxy.IRanges(x@ranges),
      strand = as_biocmask_s4_proxy.Rle(x@strand)#,
      # elementMetadata = elementMetadata_proxy(x)
    ),
    .class = class(x)
  )
}

restore_biocmask_s4_proxy.GRanges_proxy <- function(x) {
  proxy <- vec_proxy(x)
  out <- GenomicRanges::GRanges(
    seqnames = restore_biocmask_s4_proxy.Rle_proxy(proxy$seqnames),
    ranges = restore_biocmask_s4_proxy.IRanges_proxy(proxy$ranges),
    strand = restore_biocmask_s4_proxy.Rle_proxy(proxy$strand)
  )
  restore_elementMetadata(out, proxy)
}


as_biocmask_s4_proxy.CompressedGRangesList <- function(x) {
  p <- x@partitioning
  proxy <- as_biocmask_s4_proxy.GRanges(x@unlistData)
  new_biocmask_s4_proxy(
    list(
      .data = vec_chop(
        proxy,
        indices = purrr::map2(IRanges::start(p), IRanges::end(p), `:`)
      ),
      # elementMetadata = elementMetadata_proxy(x),
      partitioning = as_tibble(as.data.frame(p))
    ),
    .class = class(x)
  )
}

restore_biocmask_s4_proxy.CompressedGRangesList_proxy <- function(x) {

  # proxy <- vec_proxy(x)
  partitioning <- field(x, "partitioning")
  # mcol <- proxy$elementMetadata
  # x <- unclass(x)
  data_ <- field(x, ".data")
  flattened <- lapply(1:3, function(i) {
    lapply(data_, .subset2, i)
  })
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
}

vec_restore.CompressedGRangesList_proxy <- function(x, to) {
  x$partitioning <- x$partitioning |>
    dplyr::mutate(
      end = cumsum(width),
      start = c(0L, end[-dplyr::n()]) + 1L
    )
  NextMethod()
}

