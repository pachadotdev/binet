test_that("balassa_index works with a data frame", {
  bi <- balassa_index(
    data = trade, source = "country", target = "product", value = "export_value"
  )

  expect_is(bi, "data.frame")
  expect_equal(nrow(bi), 44435)
  expect_equal(min(bi$value), 0)
  expect_equal(max(bi$value), 1)
})

test_that("balassa_index returns error with vector data", {
  expect_error(
    balassa_index(
      data = as.numeric(trade$export_value),
      source = "country",
      target = "product",
      value = "export_value"
    )
  )
})

test_that("balassa_index returns error with numeric source/target", {
  expect_error(
    balassa_index(
      data = trade,
      source = 200,
      target = 100,
      value = "export_value"
    )
  )
})

test_that("balassa_index returns error with character discrete", {
  expect_error(
    balassa_index(
      data = trade,
      source = "country",
      target = "product",
      value = "export_value",
      discrete = "yes"
    )
  )
})

test_that("balassa_index returns error with character cutoff", {
  expect_error(
    balassa_index(
      data = trade,
      source = "country",
      target = "product",
      value = "export_value",
      cutoff = "one"
    )
  )
})
