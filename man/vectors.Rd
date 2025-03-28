% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bioc-.R
\docType{data}
\name{vectors}
\alias{vectors}
\alias{class_vec}
\alias{class_vctrs_vec}
\alias{class_s4_vec}
\alias{class_DF}
\alias{class_df}
\title{classes for vctrs and S4 Vectors}
\format{
An object of class \code{classRepresentation} of length 1.

An object of class \code{classRepresentation} of length 1.

An object of class \code{classRepresentation} of length 1.

An object of class \code{classRepresentation} of length 1.

An object of class \code{classRepresentation} of length 1.
}
\usage{
class_vec

class_vctrs_vec

class_s4_vec

class_DF

class_df
}
\value{
S4 class union or base class
}
\description{
A set of classes definitions and Class unions that help establish S4 method
dispatch.
These classes were made to re-export several \code{vctrs} functions such that
internals for \code{biocmask} were consistent with room for optimization.
}
\seealso{
\code{\link[=bioc_rep]{bioc_rep()}},\code{\link[=bioc_recycle]{bioc_recycle()}},\code{\link[=bioc_slice]{bioc_slice()}}
}
\keyword{datasets}
