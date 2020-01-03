test_that("complexity measures are aligned with the expected output", {
  cm <- complexity_measures(
    x = "country",
    y = "product",
    binet_output$balassa_index,
    tbl = T
  )

  expect_is(cm, "list")
  expect_is(cm$complexity_index_x, "data.frame")
  expect_is(cm$complexity_index_y, "data.frame")
  expect_is(cm$diversity, "data.frame")
  expect_is(cm$ubiquity, "data.frame")
  expect_equal(nrow(cm$complexity_index_x), 158)
  expect_equal(nrow(cm$complexity_index_y), 991)
  expect_equal(nrow(cm$diversity), 158)
  expect_equal(nrow(cm$ubiquity), 991)
})
