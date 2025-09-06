# as_vec <- function(x) {
#   x <- as.vector(x)
#   # unintuitively, a matrix of lists does NOT
#   # become a vector after `as.vector()`,
#   # must remove dim
#   dim(x) <- NULL
#   x
# }
#
#
# prepend_rownames <- function(DF, column) {
#   name_col <- rownames(DF) %||% as.character(seq_len(nrow(DF)))
#   name_col <- list(name_col)
#   names(name_col) <- column
#   DF@listData <- c(name_col, DF@listData)
#   DF
# }

#' @export
new_DF <- function(
    .data,
    nrows = do.call(bioc_size_common, .data),
    rownames = NULL) {
  S4Vectors::new2(
    "DFrame",
    listData = .data,
    nrows = nrows,
    rownames = rownames,
    check = FALSE
  )
}

#' @export
as_DF <- function(.data, rownames = NULL) {
  size <- do.call(bioc_size_common, .data)
  .data <- lapply(.data, biocmask::bioc_recycle, size = size)
  new_DF(
    .data = .data,
    nrows = size,
    rownames = rownames
  )
}
