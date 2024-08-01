#test

library(rlang)
library(tidyr)
library(vctrs)
library(dplyr)
library(SummarizedExperiment)

# devtools::load_all()
# sys.source("R/grouped_list.R", envir = attach(NULL, name = "grouped_list"))
sys.source("R/group_by.R", envir = attach(NULL, name = "group_by_funs"))
sys.source("R/mask_env_top.R", envir = attach(NULL, name = "mask_envs"))
sys.source("R/prepare_quos.R", envir = attach(NULL, name = "prep_quo"))
sys.source("R/biocmask-R6.R", envir = attach(NULL, name = "biocmask-R6"))
sys.source("R/connect_masks.R", envir = attach(NULL, name = "connect"))
sys.source("R/biocmask-R6-manager.R", envir = attach(NULL, name = "biocmask-manager-R6"))
sys.source("R/biocmask.R", envir = attach(NULL, name = "biocmask"))

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
metadata(se)[["group_data"]] <- groups_data

mutate(se, counts_1 = counts + 1)

ind <- pull_group_indices(groups_data)

chop_data <- vec_chop_assays(assay(se,"counts"), ind)
id <- 1L
filter(ind, .rows_group_id == .env$id) |>
  select(.group_id, .col_keys) |>
  unnest(.col_keys) |> {
  \(x, chop_data) {
    new_grouped_lst(
      list(chop_data[x[[1L]]]),
      keys = x[-1L]
    )
  }
}(chop_data)
