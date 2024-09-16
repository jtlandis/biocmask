

test_that("summarize works - no groups", {
  
  res <- expect_no_error(
    summarize(
      se_simple,
      sum = sum(counts),
      rows(sum = sum(length)),
      cols(sum = sum(1:n()))
    )
  )
  
})