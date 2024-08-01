#test

library(rlang)
library(tidyr)
library(vctrs)
library(dplyr)
library(SummarizedExperiment)

# devtools::load_all()
# sys.source("R/grouped_list.R", envir = attach(NULL, name = "grouped_list"))
source("R/group_by.R") #, envir = attach(NULL, name = "group_by_funs"))
source("R/mask_env_top.R")  #,envir = attach(NULL, name = "mask_envs"))
source("R/prepare_quos.R")  #,envir = attach(NULL, name = "prep_quo"))
source("R/biocmask-R6.R")  #,envir = attach(NULL, name = "biocmask-R6"))
source("R/connect_masks.R")  #,envir = attach(NULL, name = "connect"))
source("R/biocmask-R6-manager.R")  #,envir = attach(NULL, name = "biocmask-manager-R6"))
source("R/biocmask.R")  #,envir = attach(NULL, name = "biocmask"))
source("R/mutate.R")

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

mutate(se, 
       counts_1 = counts + 1,
       logcounts = log(counts_1),
       rows(sum = rowSums(.assays_asis$counts)),
       cols(sum = purrr::map_dbl(.assays$counts, sum)))
gse <- group_by(se,
         cols(condition),
         rows(direction))
gse |> mutate(
    n = n(),
    cols(n = n()),
    rows(n = n())
  ) |> rowData()

# for (X in groups_data$row_groups$.indices)


