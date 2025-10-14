# The following variables are referred to via NSE syntax
# somewhere in biocmask. As such, they show up as NOTES in
# R CMD Check. This is to satisfy R CMD Check

`biocmask:::ctx:::n_groups` <- NULL
`biocmask:::ctx:::group_id` <- NULL
`biocmask:::ctx` <- NULL


.onLoad <- function(...) {
  register_with_package(
    "IRanges",
    register_iranges_init
  )
  register_with_package(
    "GenomicRanges",
    register_granges_init
  )
}
