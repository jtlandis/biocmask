# The following variables are referred to via NSE syntax
# somewhere in biocmask. As such, they show up as NOTES in
# R CMD Check. This is to satisfy R CMD Check
name <- NULL
.features <- NULL
.samples <- NULL
.rows_group_id <- NULL
key <- loc <- NULL

## from mask_env_top.R
## these bindings are ceated in the data-mask
`biocmask:::ctx::nrow` <- NULL
`biocmask:::ctx::ncol` <- NULL
.group_id <- NULL
`.rows::.indices_group_id` <- NULL
`.cols::.indices_group_id` <- NULL
`biocmask:::ctx:::n_groups` <- NULL
`biocmask:::ctx` <- NULL
`biocmask:::ctx:::group_chop_ids` <- NULL
`biocmask:::ctx:::group_id` <- NULL
`biocmask:::ctx:::ncol` <- NULL
`biocmask:::ctx:::nrow` <- NULL
`biocmask:::assays:::group_chop_ids` <- NULL
`biocmask:::rows:::group_chop_ids` <- NULL
`biocmask:::cols:::group_chop_ids` <- NULL
`biocmask:::dim:::nrow` <- NULL
`biocmask:::dim:::ncol` <- NULL
`biocmask:::dim:::size` <- NULL
`biocmask:::dim:::n` <- NULL