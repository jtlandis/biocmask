

test_that("summarize works - no groups", {
  
  res <- expect_no_error(
    summarize(
      se_simple,
      sum = sum(counts),
      rows(sum = sum(length)),
      cols(sum = sum(1:n()))
    )
  )
  
  #outputs should be 1 x 1
  expect_identical(dim(res), c(1L, 1L))
})

test_that("summarize works - groups", {
  
  gse <- group_by(se_simple, rows(direction))
  
  res <- expect_no_error(
    summarize(
      gse,
      sum = sum(counts),
      rows(sum = sum(length)),
      cols(sum = colSums(.assays_asis$counts))
    )
  )
  
  #outputs should be 1 x 1
  expect_identical(dim(res), c(1L, 1L))
})
