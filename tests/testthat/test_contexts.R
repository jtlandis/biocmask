is_pronoun <- function(x) inherits(x, "rlang_data_pronoun")
biocmask_eval_ctx <- function(
  ...,
  .ctx,
  .data,
  .results = c("chops", "results")
) {
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
eval_ctx_ungrouped <- function(..., .ctx, .data) {
  biocmask_eval_ctx(
    ...,
    .ctx = .ctx,
    .data = .data
  ) |>
    lapply(`[[`, 1L)
}

test_that("pronouns reconstruct correct order", {
  #res <- summarize(
  #  group_by(small, rows(y), cols(z)),
  #  check = list(x),
  #  rows(check = list(.assays_asis$x)),
  #  cols(check = list(.assays_asis$x))
  #)
  ## we should expect rownames for rows$check to be split by group
  #expect_identical(lapply(rowData(res)$check, rownames), list(c("A","C"), "B"))
  ## we should expect rownames for cols$check to be in correct order
  #expect_identical(lapply(colData(res)$check, rownames), list(c("A","B","C"),
  #                                                            c("A","B","C")))
  #
  ## we should expect colnames for rows$check to be in correct order
  #expect_identical(lapply(rowData(res)$check, colnames),
  #                 list(c("D","E","F","G"),
  #                      c("D","E","F","G")))
  ## we should expect colnames for cols$check to be split by group
  #expect_identical(lapply(colData(res)$check, colnames),
  #                 list(c("D","G"),c("E","F")))
})
