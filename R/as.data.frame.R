
#' @name as.data.frame
#' @title create data.frame
#' @param x `SummarizedExperiment` object
#' @param ... unused arguments
#' @export
as.data.frame.SummarizedExperiment <- function(x, ...) {
  nc <- ncol(x)
  nr <- nrow(x)
  nn <- nc * nr
  .features <- rownames(x) %||% seq_len(nr)
  .samples <- colnames(x) %||% seq_len(nc)
  out <- c(
    list(
      .features = vctrs::vec_rep(.features, times = nc),
      .samples = vctrs::vec_rep_each(.samples, times = nr)
    ),
    lapply(assays(x), as_vec),
    lapply(rowData(x), vctrs::vec_rep, times = nc),
    lapply(colData(x), vctrs::vec_rep_each, times = nr)
  )
  attr(out, "row.names") <- c(NA_integer_, - nn)
  class(out) <- "data.frame"
  out
  
}
