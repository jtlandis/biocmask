library("testthat")
library("biocmask")

biocmask_eval_ctx <- function(..., .ctx, .data, .results = c("chops", "results")) {
  bm <- new_biocmask_manager(.data)
  quos <- biocmask_quos(..., .ctx_default = .ctx)
  bm$ctx <- .ctx
  lapply(quos, bm$eval)
  .results <- match.arg(.results, choices = c("chops", "results"))
  switch(
    .results,
    chops = {
      # internal to get chops as-is without forcing data
      biocmask:::get_mask_chops(bm)[[.ctx]]
    },
    results = {
      bm$results()[[.ctx]]
    }
  )
}

test_check("biocmask")
