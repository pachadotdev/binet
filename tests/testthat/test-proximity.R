test_that("proximity results are aligned with the expected output ", {
  pr <- proximity(
    bi = binet_output$balassa_index,
    x = "country",
    y = "product",
    v = "value",
    d = binet_output$complexity_measures$diversity,
    dx = "country",
    u = binet_output$complexity_measures$ubiquity,
    uy = "product",
    tbl = T
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_x), 10432)
  expect_equal(nrow(pr$proximity_y), 417822)
  expect_gte(min(pr$proximity_x$value), 0)
  expect_lte(max(pr$proximity_x$value), 1)
  expect_gte(min(pr$proximity_y$value), 0)
  expect_lte(max(pr$proximity_y$value), 1)
})
