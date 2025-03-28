source("../source.r")
bm <- df_simple_mask()
gbm <- df_simple_grouped_mask()

test_that("evaluation works", {
  expect_error(bm$eval(rlang::quo("foo")), "requires quosure from")
  quo <- biocmask_quos(
    foo = rep(1L, 20L),
    metadata(foo = rep(1L, 5L)),
    .ctx_default = "default",
    .ctx_opt = "metadata"
  )
  bm$eval(quo[[1]])
  bm$ctx <- "metadata"
  bm$eval(quo[[2]])
  res <- bm$results()
  expect_identical(sum(res$default$foo), 20L)
  expect_identical(sum(res$metadata$foo), 5L)
})
