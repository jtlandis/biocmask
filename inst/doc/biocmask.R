## ----message=FALSE, warning=FALSE---------------------------------------------
library(airway)
data(airway)
library(dplyr)

## ----echo=FALSE---------------------------------------------------------------
# clean up the metadata a bit to make it easier to view
# colData(airway) <- colData(airway)[,1:4]
# rowData(airway) <- rowData(airway)[,c(1:2,4)]

## ----message=FALSE------------------------------------------------------------
library(biocmask)
# add data (mutate) to any of the three tables,
# assay, colData, rowData,
# ...using contextual helpers cols() and rows()
# airway |>
#   mutate(log_counts = log1p(counts),
#          cols(treated = dex == "trt"),
#          rows(new_id = paste0("gene-", gene_name)))

## -----------------------------------------------------------------------------
airway$sizeFactor <- runif(8, .9, 1.1)

# making scaled counts, then computing row means:
# airway |>
#   mutate(scaled_counts = counts / .cols$sizeFactor, #
#          rows(ave_scl_cts = rowMeans(.assays_asis$scaled_counts)))

## -----------------------------------------------------------------------------
# airway |>
#   mutate(scaled_counts = counts / .cols$sizeFactor,
#          # You may expect a list when accessing other contexts
#          # from either the rows() or cols() contexts.
#          rows(ave_scl_cts = purrr::map_dbl(.assays$scaled_counts, mean)))

## -----------------------------------------------------------------------------
# summary <- airway |>
#   group_by(rows(gene_biotype)) |>
#   summarize(col_sums = colSums(counts),
#             # may rename rows with .features
#             rows(.features = unique(gene_biotype)))
# # summarize returns a SummarizedExperiment here,
# # retaining rowData and colData

# summary |> rowData()

# # visualizing the output as a tibble:
# library(tibble)
# summary |>
#   pull(col_sums) |>
#   as_tibble(rownames = "type")

## -----------------------------------------------------------------------------
knitr::include_graphics("../man/figures/Overview-bindings.png")

## -----------------------------------------------------------------------------
knitr::include_graphics("../man/figures/Overview-pronouns.png")

## -----------------------------------------------------------------------------
# here the `.cols$` pronoun reshapes the data to fit the `assays` context
# airway |>
#   mutate(scaled_counts = counts / .cols$sizeFactor)

# # this is equivalent to the following, where we have to transpose
# # the `counts` assay data twice to enable the correct recycling
# # of the size factor vector
# airway |>
#   mutate(scaled_counts = t(t(counts) / .cols_asis$sizeFactor))

## -----------------------------------------------------------------------------
knitr::include_graphics("../man/figures/contextual_groups.png")

## ----eval=FALSE---------------------------------------------------------------
# group_by(se_simple, rows(direction), cols(condition)) |>
#   mutate(rows(data_from_cols = .cols$condition))

## ----eval=FALSE---------------------------------------------------------------
# options("show_SummarizedExperiment_as_tibble_abstraction" = TRUE)

## -----------------------------------------------------------------------------
# se_simple
# # moving data to rownames and colnames
# se_simple |>
#   mutate(
#     orig_names = sprintf(
#       "%s-%s",
#       # .features is installed in the rows() context
#       .rows$.features,
#       # .samples is installed in the cols() context
#       .cols$.samples),
#     rows(.features = gene,
#          # deleting rowData column
#          gene = NULL),
#     cols(.samples = sample,
#          # deleting colData column
#          sample = NULL)
#   )


## ----eval=FALSE---------------------------------------------------------------
# .data |>
#   mutate(foo = bar) |>
#   mutate(baz = foo^2)

## ----eval=FALSE---------------------------------------------------------------
# # .data |>
# #   mutate(
# #     foo = bar,
# #     baz = foo^2
# #   )

## -----------------------------------------------------------------------------
devtools::session_info()

