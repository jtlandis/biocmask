
test_that("arrange works - ungrouped", {
  
  expect_error(
    arrange(
      se_simple, counts
    )
  )
  
  res <- expect_no_error(
    arrange(
      se_simple,
      rows(length),
      cols(dplyr::desc(sample))
    )
  )
  
  expect_identical(rowData(res)[["direction"]],
                   c("-","+","+","-","+"))
  expect_identical(colData(res)[["condition"]],
                   c("drug","drug", "cntrl","cntrl"))
})


test_that("arrange works - grouped", {
  gse <- group_by(se_simple, rows(direction), cols(condition))
  
  res <- expect_no_error(
    arrange(
      gse,
      rows(length),
      cols(dplyr::desc(sample))
    )
  )
  
  expect_identical(rowData(res)[["direction"]],
                   c("-","+","+","-","+"))
  expect_identical(colData(res)[["condition"]],
                   c("drug","drug", "cntrl","cntrl"))
})

test_that("arrange works - grouped .by_group = TRUE", {  
  browser()
  gse <- group_by(se_simple, rows(direction), cols(condition))
  # testing .by_group = TRUE
  res <- 
    arrange(
      gse,
      rows(length),
      cols(dplyr::desc(sample)),
      .by_group = TRUE
    )
  
  
  expect_identical(rowData(res)[["direction"]],
                   c("+","+","+","-","-"))
  expect_identical(colData(res)[["condition"]],
                   c("cntrl","cntrl","drug","drug"))
})