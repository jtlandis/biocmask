% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biocmask-R6.R
\name{biocmask}
\alias{biocmask}
\title{\code{biocmask} Data Mask Object}
\value{
an R6 object of class \code{biocmask}
}
\description{
An R6 Object that tracks bindings of a list-like object.
This includes DFrame objects. There are several inherited
environments that the data is stored within.

Environments:

.shared_env --> curr_group_ctx --> foreign --> lazy --> chops --> active_mask
\itemize{
\item .shared_env : environment provided at initialization. This may be shared
with multiple other BiocDataMasks.
\item curr_group  : Currently not used.
\item foreign     : space to put foreign bindings, i.e. object unrelated to \code{.data}
provided at initialization. This space is currently used to
place the pronouns into related contexts.
\item lazy        : A strict lazy binding to the data within \code{.data}. This binding
is made only at initialization.
\item chops       : lazy data but chopped into list by \code{.indices}. New bindings
for this BiocDataMask context are expected to be in a
"chopped" format and are assigned here.
\item active_mask : An active binding to chops in which the proper list index is
used depending on the current group context. The current group
context is at this moment determined by the .shared_env NOT
the curr_group. I have plans to remove the curr_group
environment.
}
}
\examples{
# note: this R6 class is not exported at this moment

mask <- getNamespace("biocmask")$biocmask$new(iris,
                     .env_bot = rlang::env(`biocmask:::ctx:::group_id` = 1L))
mask$eval(quote(Sepal.Width))



}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{environments}}{the hierarchy of environments for this mask}

\item{\code{names}}{the associated names of data in mask}

\item{\code{ptype}}{get zero-length slices of underlying data}

\item{\code{added}}{newly added names to the mask
holds grouping information
for this object
foreign bindings}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-biocmask-new}{\code{biocmask$new()}}
\item \href{#method-biocmask-on_bind}{\code{biocmask$on_bind()}}
\item \href{#method-biocmask-bind}{\code{biocmask$bind()}}
\item \href{#method-biocmask-unchop}{\code{biocmask$unchop()}}
\item \href{#method-biocmask-results}{\code{biocmask$results()}}
\item \href{#method-biocmask-eval}{\code{biocmask$eval()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-biocmask-new"></a>}}
\if{latex}{\out{\hypertarget{method-biocmask-new}{}}}
\subsection{Method \code{new()}}{
Create a biocmask from \code{.data}. \code{.data} is chopped by
\code{.indices}, and environments are built from \code{.env}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{biocmask$new(.data, .indices = NULL, .env_bot, .env_top = .env_bot)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{.data}}{a named list like object to create a mask}

\item{\code{.indices}}{the indices that will be used to chop \code{.data}}

\item{\code{.env_bot}}{an environment that the resulting mask will be built from.}

\item{\code{.env_top}}{an environment that \code{.env_bot} inherits from}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-biocmask-on_bind"></a>}}
\if{latex}{\out{\hypertarget{method-biocmask-on_bind}{}}}
\subsection{Method \code{on_bind()}}{
appends a callback function that is executed after a value is bound
to this mask. Mainly used to inform other masks of new values
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{biocmask$on_bind(.fun)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{.fun}}{a function created from \code{add_bind()}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-biocmask-bind"></a>}}
\if{latex}{\out{\hypertarget{method-biocmask-bind}{}}}
\subsection{Method \code{bind()}}{
binds value to an name within the chops environment.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{biocmask$bind(name, value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{a character scalar}

\item{\code{value}}{results from \verb{$eval} in the form of chops}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-biocmask-unchop"></a>}}
\if{latex}{\out{\hypertarget{method-biocmask-unchop}{}}}
\subsection{Method \code{unchop()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{biocmask$unchop(name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{name of binding to retrieve and unchop}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-biocmask-results"></a>}}
\if{latex}{\out{\hypertarget{method-biocmask-results}{}}}
\subsection{Method \code{results()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{biocmask$results()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
named list of evaluated expression, unchopped
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-biocmask-eval"></a>}}
\if{latex}{\out{\hypertarget{method-biocmask-eval}{}}}
\subsection{Method \code{eval()}}{
evaluates a quoted expression within a new datamask
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{biocmask$eval(quo, env = caller_env())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{quo}}{a quosure to evaluate}

\item{\code{env}}{an environment to search after mask}
}
\if{html}{\out{</div>}}
}
}
}
