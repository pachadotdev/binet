test_that("proximity results are aligned with the expected output", {
  pr <- proximity(
    balassa_index = binet_output$balassa_index,
    balassa_sum_source = binet_output$complexity_measures$balassa_sum_source,
    balassa_sum_target = binet_output$complexity_measures$balassa_sum_target
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_source), 10432)
  expect_equal(nrow(pr$proximity_target), 417822)
  expect_gte(min(pr$proximity_source$value), 0)
  expect_lte(max(pr$proximity_source$value), 1)
  expect_gte(min(pr$proximity_target$value), 0)
  expect_lte(max(pr$proximity_target$value), 1)
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
  expect_equal(nrow(pr$proximity_source), 158)
  expect_equal(nrow(pr$proximity_target), 991)
  expect_gte(min(pr$proximity_source), 0)
  expect_lte(max(pr$proximity_source), 1)
  expect_gte(min(pr$proximity_target), 0)
  expect_lte(max(pr$proximity_target), 1)
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
  expect_equal(nrow(pr$proximity_source), 10432)
  expect_gte(min(pr$proximity_source$value), 0)
  expect_lte(max(pr$proximity_source$value), 1)
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
  expect_equal(nrow(pr$proximity_target), 417822)
  expect_gte(min(pr$proximity_target$value), 0)
  expect_lte(max(pr$proximity_target$value), 1)
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
