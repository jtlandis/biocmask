log_registration <- function(pkg) {
  rlang::inform(sprintf("registering methods for `%s`", pkg))
}
register_with_package <- function(pkg, fn) {
  force(fn)
  force(pkg)
  f <- function(...) {
    log_registration(pkg)
    fn()
  }
  if (isNamespaceLoaded(pkg)) {
    f()
  } else {
    setHook(packageEvent(pkg, "onLoad"), f)
  }
  invisible(NULL)
}

register_iranges_init <- function() {
  S7::method(bioc_init, methods::getClass("IRanges", where = "IRanges")) <-
    function(x, size = 0L, ...) {
      out <- IRanges::IRanges(
        start = integer(size),
        width = integer(size)
      )
      S4Vectors::mcols(out) <- bioc_init(S4Vectors::mcols(x), size = size)
      out
    }
  S7::method(bioc_init, methods::getClass("IPos", where = "IRanges")) <-
    function(x, size = 0L, ...) {
      out <- IRanges::IPos(
        pos = integer(size)
      )
      S4Vectors::mcols(out) <- bioc_init(S4Vectors::mcols(x), size = size)
      out
    }
}

register_granges_init <- function() {
  S7::method(
    bioc_init,
    methods::getClass("GRanges", where = "GenomicRanges")
  ) <- function(x, size = 0L, ...) {
    out <- GenomicRanges::GRanges(
      seqnames = S4Vectors::Rle(
        values = "NA",
        lengths = size
      ),
      ranges = bioc_init(IRanges::ranges(x), size = size),
      strand = S4Vectors::Rle(
        values = factor("*", levels = c("+", "-", "*")),
        lengths = size
      )
    )
    S4Vectors::mcols(out) <- bioc_init(S4Vectors::mcols(x), size = size)
    out
  }
}
