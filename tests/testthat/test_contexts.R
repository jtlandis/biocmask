
is_pronoun <- function(x) inherits(x, "rlang_data_pronoun")
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
eval_ctx_ungrouped <- function(..., .ctx, .data) {
  biocmask_eval_ctx(
    ...,
    .ctx = .ctx,
    .data = .data
  ) |> lapply(`[[`, 1L)
}

test_that("pronouns are available within assays context", {

  res <- eval_ctx_ungrouped(
    pronoun_cols = is_pronoun(.cols),
    pronoun_rows = is_pronoun(.rows),
    pronoun_cols_asis = is_pronoun(.cols_asis),
    pronoun_rows_asis = is_pronoun(.rows_asis),
    .ctx = "assays",
    .data = se_simple)
  
  # to dimensions
  expect_true(res$pronoun_rows)
  expect_true(res$pronoun_cols)
  
  #asis pronouns
  expect_true(res$pronoun_rows_asis)
  expect_true(res$pronoun_cols_asis)
  
})

test_that("pronouns are available within rows context", {
  
  res <- eval_ctx_ungrouped(
    pronoun_assays = is_pronoun(.assays),
    pronoun_cols = is_pronoun(.cols),
    pronoun_assays_asis = is_pronoun(.assays_asis),
    pronoun_cols_asis = is_pronoun(.cols_asis),
    .ctx = "rows",
    .data = se_simple
  ) 
  
  expect_true(res$pronoun_assays)
  expect_true(res$pronoun_cols)
  
  expect_true(res$pronoun_assays_asis)
  expect_true(res$pronoun_cols_asis)
  
})

test_that("pronouns are available within cols context", {
  
  res <- eval_ctx_ungrouped(
    pronoun_assays = is_pronoun(.assays),
    pronoun_rows = is_pronoun(.rows),
    pronoun_assays_asis = is_pronoun(.assays_asis),
    pronoun_rows_asis = is_pronoun(.rows_asis),
    .ctx = "cols",
    .data = se_simple
  ) 
  
  expect_true(res$pronoun_assays)
  expect_true(res$pronoun_rows)
  
  expect_true(res$pronoun_assays_asis)
  expect_true(res$pronoun_rows_asis)
  
})


test_that("assays context: non-'asis' pronouns coerce data -> matrix-like vec",
          {
  res <- eval_ctx_ungrouped(
    cols = .cols$condition,
    rows = .rows$length,
    .ctx = "assays",
    .data = se_simple
  ) 
  
  expect_type(res$cols, "character")
  expect_type(res$rows, "integer")
  
  expect_length(res$cols, 20L)
  expect_length(res$cols, 20L)

})

test_that("rows context: non-'asis' pronouns coerce data -> list vec", {
  res <- eval_ctx_ungrouped(
    assays = .assays$counts,
    cols = .cols$condition,
    .ctx = "rows",
    .data = se_simple
  ) 
  
  expect_type(res$assays, "list")
  expect_type(res$cols, "list")
  
  expect_length(res$assays, 5L)
  expect_length(res$cols, 5L)
  # specific to row-context viewing assays
  # Each is NOT a matrix anymore, slicing drops dimensions since we are 
  # viewing element-wise.
  expect_true(
    all(vapply(res$assays, Negate(inherits), what = "matrix", FUN.VALUE = TRUE))
  )
  expect_true(
    all(vapply(res$assays, length, 1L)==4L)
  )
  # specific to row-context viewing cols
  expect_true(
    all(vapply(res$cols[-1], identical, y = res$cols[[1]], FUN.VALUE = TRUE))
  )
  
})

test_that("cols context: non-'asis' pronouns coerce data -> list vec", {
  res <- eval_ctx_ungrouped(
    assays = .assays$counts,
    rows = .rows$length,
    .ctx = "cols",
    .data = se_simple
  )
  
  expect_type(res$assays, "list")
  expect_type(res$rows, "list")
  
  expect_length(res$assays, 4L)
  expect_length(res$rows, 4L)
  # specific to col-context viewing assays
  # Each is NOT a matrix anymore, slicing drops dimensions since we are 
  # viewing element-wise.
  expect_true(
    all(vapply(res$assays, Negate(inherits), what = "matrix", FUN.VALUE = TRUE))
  )
  expect_true(
    all(vapply(res$assays, length, 1L)==5L)
  )
  # specific to col-context viewing rows
  # all elements should be identical
  expect_true(
    all(vapply(res$rows[-1], identical, y = res$rows[[1]], FUN.VALUE = TRUE))
  )
})
