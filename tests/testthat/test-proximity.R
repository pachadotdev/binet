test_that("proximity results are aligned with the expected output", {
  pr <- proximity(
    balassa_index = binet_output$balassa_index,
    balassa_sum_source = binet_output$complexity_measures$diversity,
    balassa_sum_target = binet_output$complexity_measures$ubiquity
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_x), 10432)
  expect_equal(nrow(pr$proximity_y), 417822)
  expect_gte(min(pr$proximity_x$value), 0)
  expect_lte(max(pr$proximity_x$value), 1)
  expect_gte(min(pr$proximity_y$value), 0)
  expect_lte(max(pr$proximity_y$value), 1)
})

test_that("proximity works with a matrix", {
  balassa_index_m <- binet_output$balassa_index %>%
    tidyr::spread(product, value)

  balassa_index_rownames <- dplyr::select(balassa_index_m, country) %>%
    dplyr::pull()

  balassa_index_m <- dplyr::select(balassa_index_m, -country) %>% as.matrix()
  balassa_index_m[is.na(balassa_index_m)] <- 0
  rownames(balassa_index_m) <- balassa_index_rownames

  balassa_index_m <- Matrix::Matrix(balassa_index_m, sparse = TRUE)

  pr <- proximity(
    bi = balassa_index_m,
    d = binet_output$complexity_measures$diversity,
    dx = "country",
    u = binet_output$complexity_measures$ubiquity,
    uy = "product",
    tbl = F
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_x), 158)
  expect_equal(nrow(pr$proximity_y), 991)
  expect_gte(min(pr$proximity_x), 0)
  expect_lte(max(pr$proximity_x), 1)
  expect_gte(min(pr$proximity_y), 0)
  expect_lte(max(pr$proximity_y), 1)
})

test_that("proximity returns x matrix only", {
  pr <- proximity(
    bi = binet_output$balassa_index,
    x = "country",
    y = "product",
    v = "value",
    d = binet_output$complexity_measures$diversity,
    dx = "country",
    u = binet_output$complexity_measures$ubiquity,
    uy = "product",
    tbl = T,
    mat = "x"
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_x), 10432)
  expect_gte(min(pr$proximity_x$value), 0)
  expect_lte(max(pr$proximity_x$value), 1)
})

test_that("proximity returns y matrix only", {
  pr <- proximity(
    bi = binet_output$balassa_index,
    x = "country",
    y = "product",
    v = "value",
    d = binet_output$complexity_measures$diversity,
    dx = "country",
    u = binet_output$complexity_measures$ubiquity,
    uy = "product",
    tbl = T,
    mat = "y"
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_y), 417822)
  expect_gte(min(pr$proximity_y$value), 0)
  expect_lte(max(pr$proximity_y$value), 1)
})

test_that("proximity returns an error with vector bi", {
  expect_error(
    proximity(
      bi = binet_output$balassa_index$value,
      x = "country",
      y = "product",
      v = "value",
      d = binet_output$complexity_measures$diversity,
      dx = "country",
      u = binet_output$complexity_measures$ubiquity,
      uy = "product",
      tbl = T
    )
  )
})

test_that("proximity returns an error with NULL d", {
  expect_error(
    proximity(
      bi = binet_output$balassa_index$value,
      x = "country",
      y = "product",
      v = "value",
      d = NULL,
      dx = "country",
      u = binet_output$complexity_measures$ubiquity,
      uy = "product",
      tbl = T
    )
  )
})

test_that("proximity returns an error with NULL d", {
  expect_error(
    proximity(
      bi = binet_output$balassa_index$value,
      x = "country",
      y = "product",
      v = "value",
      d = binet_output$complexity_measures$diversity,
      dx = "country",
      u = NULL,
      uy = "product",
      tbl = T
    )
  )
})

test_that("proximity returns an error with character tbl", {
  expect_error(
    proximity(
      bi = binet_output$balassa_index,
      x = "country",
      y = "product",
      v = "value",
      d = binet_output$complexity_measures$diversity,
      dx = "country",
      u = binet_output$complexity_measures$ubiquity,
      uy = "product",
      tbl = "yes"
    )
  )
})

test_that("proximity returns an error with character pro", {
  expect_error(
    proximity(
      bi = binet_output$balassa_index,
      x = "country",
      y = "product",
      v = "value",
      d = binet_output$complexity_measures$diversity,
      dx = "country",
      u = binet_output$complexity_measures$ubiquity,
      uy = "product",
      tbl = T,
      pro = 1
    )
  )
})

test_that("proximity returns an error with character D", {
  expect_error(
    proximity(
      bi = binet_output$balassa_index,
      x = "country",
      y = "product",
      v = "value",
      d = "diversity",
      dx = "country",
      u = binet_output$complexity_measures$ubiquity,
      uy = "product",
      tbl = T,
      pro = "both"
    )
  )
})
