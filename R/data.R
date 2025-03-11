#' @title Biocmask Simple Example Summarized Experiment
#'
#' @description
#' A small DataFrame Object of 8 observations, and 3 columns
#'
#' @format ## `df_simple`
#' \describe{
#'  \item{`signal`}{sampled data points between 1:100}
#'  \item{condition}{an Rle object of "cntrl" and "drug"}
#'  \item{group}{a factor vector of "A", "B", "C"}
#' }
#' @return a `SummarizedExperiment` object
#' @examples
#' df_simple |> names()
"df_simple"
