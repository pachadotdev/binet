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
    tidyr::spread(target, value)

  balassa_index_rownames <- dplyr::select(balassa_index_m, source) %>%
    dplyr::pull()

  balassa_index_m <- dplyr::select(balassa_index_m, -source) %>% as.matrix()
  balassa_index_m[is.na(balassa_index_m)] <- 0
  rownames(balassa_index_m) <- balassa_index_rownames

  balassa_index_m <- Matrix::Matrix(balassa_index_m, sparse = TRUE)

  pr <- proximity(
    balassa_index = balassa_index_m,
    balassa_sum_source = binet_output$complexity_measures$balassa_sum_source,
    balassa_sum_target = binet_output$complexity_measures$balassa_sum_target,
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_source), 10432)
  expect_equal(nrow(pr$proximity_target), 417822)
  expect_gte(min(pr$proximity_source$value), 0)
  expect_lte(max(pr$proximity_source$value), 1)
  expect_gte(min(pr$proximity_target$value), 0)
  expect_lte(max(pr$proximity_target$value), 1)
})

test_that("proximity returns source proximity only", {
  pr <- proximity(
    balassa_index = binet_output$balassa_index,
    balassa_sum_source = binet_output$complexity_measures$balassa_sum_source,
    balassa_sum_target = binet_output$complexity_measures$balassa_sum_target,
    compute = "source"
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_source), 10432)
  expect_equal(nrow(pr$proximity_target), NULL)
  expect_gte(min(pr$proximity_source$value), 0)
  expect_lte(max(pr$proximity_source$value), 1)
})

test_that("proximity returns target proximity only", {
  pr <- proximity(
    balassa_index = binet_output$balassa_index,
    balassa_sum_source = binet_output$complexity_measures$balassa_sum_source,
    balassa_sum_target = binet_output$complexity_measures$balassa_sum_target,
    compute = "target"
  )

  expect_is(pr, "list")
  expect_equal(nrow(pr$proximity_source), NULL)
  expect_equal(nrow(pr$proximity_target), 417822)
  expect_gte(min(pr$proximity_target$value), 0)
  expect_lte(max(pr$proximity_target$value), 1)
})
