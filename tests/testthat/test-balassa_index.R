test_that("balassa_index matrix/tibble fulfills desired properties with data frames", {
  bi <- balassa_index(
    d = trade_1962,
    x = "country",
    y = "product",
    v = "value",
    tbl = T
  )

  expect_is(bi, "data.frame")
  expect_equal(nrow(bi), 44435)
  expect_equal(min(bi$value), 0)
  expect_equal(max(bi$value), 1)
})

test_that("balassa_index matrix/tibble fulfills desired properties with a matrix", {
  trade_1962_m <- trade_1962 %>%
    tidyr::spread(product, value)

  trade_1962_rownames <- dplyr::select(trade_1962_m, country) %>%
    dplyr::pull()

  trade_1962_m <- dplyr::select(trade_1962_m, -country) %>% as.matrix()
  trade_1962_m[is.na(trade_1962_m)] <- 0
  rownames(trade_1962_m) <- trade_1962_rownames

  trade_1962_m <- Matrix::Matrix(trade_1962_m, sparse = TRUE)

  bi <- balassa_index(
    d = trade_1962_m,
    x = "country",
    y = "product",
    v = "value",
    tbl = F
  )

  expect_is(bi, "dgCMatrix")
  expect_equal(nrow(bi), 158)
  expect_equal(ncol(bi), 991)
  expect_equal(min(bi), 0)
  expect_equal(max(bi), 1)
})

test_that("balassa_index returns error with vector input", {
  expect_error(
    balassa_index(
      d = as.numeric(trade_1962$value),
      x = "country",
      y = "product",
      v = "value",
      tbl = T
    )
  )
})

test_that("balassa_index returns error with numeric colnames", {
  expect_error(
    balassa_index(
      d = trade_1962,
      x = 200,
      y = 100,
      v = "value",
      tbl = T
    )
  )
})

test_that("balassa_index returns error with character discrete", {
  expect_error(
    balassa_index(
      d = trade_1962,
      x = "country",
      y = "product",
      v = "value",
      dis = "yes",
      tbl = T
    )
  )
})

test_that("balassa_index returns error with character cutoff", {
  expect_error(
    balassa_index(
      d = trade_1962,
      x = "country",
      y = "product",
      v = "value",
      cut = "two",
      tbl = T
    )
  )
})

test_that("balassa_index returns error with character tibble", {
  expect_error(
    balassa_index(
      d = trade_1962,
      x = "country",
      y = "product",
      v = "value",
      cut = 1,
      tbl = "yes"
    )
  )
})
