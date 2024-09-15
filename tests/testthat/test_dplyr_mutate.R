
test_that("mutate works - no groups", {
  
  res <- expect_no_error(
    mutate(se_simple,
           foo = 1:n(),
           rows(foo = 1:n()),
           cols(foo = 1:n()))
  )
  
  expect_identical(assay(res, "foo") |> unname(),
                   matrix(1:20, nrow = 5, ncol = 4))
  expect_identical(rowData(res)[["foo"]], 1:5)
  expect_identical(colData(res)[["foo"]], 1:4)
  
})

test_that("mutate works - with groups", {
  gse <- group_by(se_simple, rows())
})