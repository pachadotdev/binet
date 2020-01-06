test_that("balassa_index works with a data frame", {
  bi <- balassa_index(
    data = trade, source = "country", target = "product", value = "export_value"
  )

  expect_is(bi, "data.frame")
  expect_equal(nrow(bi), 44435)
  expect_equal(min(bi$value), 0)
  expect_equal(max(bi$value), 1)
})

test_that("balassa_index with a matrix", {
  trade_m <- trade %>%
    tidyr::spread(product, export_value)

  trade_rownames <- dplyr::select(trade_m, country) %>%
    dplyr::pull()

  trade_m <- dplyr::select(trade_m, -country) %>% as.matrix()
  trade_m[is.na(trade_m)] <- 0
  rownames(trade_m) <- trade_rownames

  trade_m <- Matrix::Matrix(trade_m, sparse = TRUE)

  bi <- balassa_index(
    data = trade_m, source = "country", target = "product", value = "export_value"
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
