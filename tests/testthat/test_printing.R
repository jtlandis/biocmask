

test_that("Default printing is `SummarizedExperiment`", {
  expect_snapshot(se_simple)
})

test_that("enabling `biocmask` printing", {
 
  expect_snapshot({use_show_tidy(); se_simple})
})

test_that("re-enabling default printing", {
  
  expect_snapshot({use_show_default(); se_simple})
})

test_that("force `biocmask` printing", {
  expect_snapshot(show_tidy(se_simple))
})