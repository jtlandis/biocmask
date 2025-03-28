source("../source.r")
bm <- df_simple_mask()
gbm <- df_simple_grouped_mask()

test_that("`biocmask_manager$new()` returns an R6 `biocmask_manager`", {
  expect_s3_class(bm, "R6")
  expect_s3_class(bm, "biocmask_manager")
  expect_s3_class(gbm, "R6")
  expect_s3_class(gbm, "biocmask_manager")
})

test_that("default context is `default` as specified", {
  expect_identical(bm$ctx, "default")
  expect_identical(gbm$ctx, "default")
})

test_that("context may be changed between `default` and `metadata`", {
  bm$ctx <- "metadata"
  expect_identical(bm$ctx, "metadata")
  bm$ctx <- "default"
  # shouldnt be able to change it to something else
  expect_error(bm$ctx <- "foo", "should be one of")
  # bm$ctx should still be "default" after the above failure
  expect_identical(bm$ctx, "default")
})

test_that("correct number of groups for ungrouped and grouped `df_simple`", {
  expect_identical(bm$n_groups, 1L)
  expect_identical(gbm$n_groups, 2L)
  gbm$ctx <- "metadata"
  expect_identical(gbm$n_groups, 1L)
  gbm$ctx <- "default"
})
