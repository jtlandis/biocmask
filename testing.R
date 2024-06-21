#test

library(rlang)
library(vctrs)
library(dplyr)
library(SummarizedExperiment)

# devtools::load_all()
sys.source("R/group_by.R", envir = attach(NULL, name = "group_by_funs"))

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


groups_data <- biocmask_groups(
  rowData(se)["direction"],
  colData(se)["condition"])

