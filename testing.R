#test

library(rlang)
library(SummarizedExperiment)
library(tidySummarizedExperiment)

sys.source("R/mask_env_top.R", envir = attach(NULL, name = "SE:envir"))
sys.source("R/DataMaskAbstraction.R", envir = attach(NULL, name = "SE:abstraction"))
sys.source("R/DataMaskSEManager.R", envir = attach(NULL, name = "SE:Manager"))


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

# locally named this so that I can compare
# speed-up improvements against old mutate.SummarizedExperiment methods
mutate_SummarizedExperiment <- function(.data, ...) {
  
  # browser()
  
  .env <- rlang::caller_env()
  .mask <- TidySEMaskManager$new(.data, .env, "mutate")
  poke_ctx_local("SE:::mask_manager", .mask)
  poke_ctx_local("SE:::dplyr_function", "mutate")
  poke_ctx_local("SE:::caller_env", .env)
  quos <- rlang::enquos(...)
  nms <- names(quos)
  for (k in seq_along(quos)) {
    quo <- quos[[k]]
    name <- nms[k]
    .mask$eval_mutate_assays(quo, name)
  }
  .mask$finalize_mutate_data(.data)
  
}

mutate_SummarizedExperiment(
  se,
  new_counts = counts + 1,
  rows(
    rowSum = rowSums(.assay$counts)
  )
)

dplyr::group_by(se, gene) |>
  dplyr::mutate(
    new_counts = counts + 1,
    rowSum = sum(counts)
  )

