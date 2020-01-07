test_that("fitness method works in complexity_measures", {
  cm <- complexity_measures(
    balassa_index = binet_output$balassa_index,
    method = "fitness",
    iterations = 10,
    extremality = 2
  )

  expect_is(cm, "list")
  expect_is(cm$complexity_index_source, "data.frame")
  expect_is(cm$complexity_index_target, "data.frame")
  expect_is(cm$balassa_sum_source, "data.frame")
  expect_is(cm$balassa_sum_target, "data.frame")
  expect_equal(nrow(cm$complexity_index_source), 158)
  expect_equal(nrow(cm$complexity_index_target), 991)
  expect_equal(nrow(cm$balassa_sum_source), 158)
  expect_equal(nrow(cm$balassa_sum_target), 991)
})

test_that("reflections method works in complexity_measures", {
  cm <- complexity_measures(
    balassa_index = binet_output$balassa_index,
    method = "reflections",
    iterations = 10,
    extremality = 2
  )

  expect_is(cm, "list")
  expect_is(cm$complexity_index_source, "data.frame")
  expect_is(cm$complexity_index_target, "data.frame")
  expect_is(cm$balassa_sum_source, "data.frame")
  expect_is(cm$balassa_sum_target, "data.frame")
  expect_equal(nrow(cm$complexity_index_source), 158)
  expect_equal(nrow(cm$complexity_index_target), 991)
  expect_equal(nrow(cm$balassa_sum_source), 158)
  expect_equal(nrow(cm$balassa_sum_target), 991)
})

test_that("eigenvalues method works in complexity_measures", {
  cm <- complexity_measures(
    balassa_index = binet_output$balassa_index,
    method = "eigenvalues",
    iterations = 10,
    extremality = 2
  )

  expect_is(cm, "list")
  expect_is(cm$complexity_index_source, "data.frame")
  expect_is(cm$complexity_index_target, "data.frame")
  expect_is(cm$balassa_sum_source, "data.frame")
  expect_is(cm$balassa_sum_target, "data.frame")
  expect_equal(nrow(cm$complexity_index_source), 158)
  expect_equal(nrow(cm$complexity_index_target), 991)
  expect_equal(nrow(cm$balassa_sum_source), 158)
  expect_equal(nrow(cm$balassa_sum_target), 991)
})

test_that("complexity_measures fails with NULL iterations", {
  expect_error(
    complexity_measures(
      balassa_index = binet_output$balassa_index,
      method = "fitness",
      iterations = NULL,
      extremality = 2
    )
  )
})

test_that("complexity_measures fails with NULL extremality", {
  expect_error(
    complexity_measures(
      balassa_index = binet_output$balassa_index,
      method = "fitness",
      iterations = 10,
      extremality = NULL
    )
  )
})
