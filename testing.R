#test

library(rlang)
library(vctrs)
library(SummarizedExperiment)

devtools::load_all()

# library(tidySummarizedExperiment)

set.seed(1234)
se <- SummarizedExperiment(
  list(counts = matrix(sample(1:20, 20), nrow = 5, ncol = 4)),
  rowData = data.frame(gene = sprintf("g%i", 1:5),
                       length = rbinom(5, 100, runif(5)),
                       direction = sample(c("-","+"), 5, T)),
  colData = data.frame(sample = sprintf("s%i", 1:4),
                       condition = rep(c("cntrl","drug"), each =2))
)
rownames(se) <- sprintf("row_%s", letters[1:5])
colnames(se) <- sprintf("col_%s", LETTERS[1:4])
assay(se, 'logcounts') <- log(assay(se, 'counts'))
se




biocmask:::mutate.SummarizedExperiment(
  se,
  new_counts = counts + 1,
  rows(
    rowSum = rowSums(.assay$counts),
    peek_counts = counts
  ),
  cols(
    colSum = colSums(.assay$counts)
  )
) -> result



dplyr::group_by(se, gene) |>
  dplyr::mutate(
    new_counts = counts + 1,
    rowSum = sum(counts)
  )

