test_that("fitness method works in complexity_measures", {
  cm <- complexity_measures(
    data = binet_output$balassa_index,
    method = "fitness",
    iterations = 10,
    extremality = 2
  )

  expect_is(cm, "list")
  expect_is(cm$complexity_index_x, "data.frame")
  expect_is(cm$complexity_index_y, "data.frame")
  expect_is(cm$balassa_sum_x, "data.frame")
  expect_is(cm$balassa_sum_y, "data.frame")
  expect_equal(nrow(cm$complexity_index_x), 158)
  expect_equal(nrow(cm$complexity_index_y), 991)
  expect_equal(nrow(cm$balassa_sum_x), 158)
  expect_equal(nrow(cm$balassa_sum_y), 991)
})

test_that("reflections method works in complexity_measures", {
  cm <- complexity_measures(
    data = binet_output$balassa_index,
    method = "reflections",
    iterations = 10,
    extremality = 2
  )

  expect_is(cm, "list")
  expect_is(cm$complexity_index_x, "data.frame")
  expect_is(cm$complexity_index_y, "data.frame")
  expect_is(cm$balassa_sum_x, "data.frame")
  expect_is(cm$balassa_sum_y, "data.frame")
  expect_equal(nrow(cm$complexity_index_x), 158)
  expect_equal(nrow(cm$complexity_index_y), 991)
  expect_equal(nrow(cm$balassa_sum_x), 158)
  expect_equal(nrow(cm$balassa_sum_y), 991)
})

test_that("eigenvalues method works in complexity_measures", {
  cm <- complexity_measures(
    data = binet_output$balassa_index,
    method = "eigenvalues",
    iterations = 10,
    extremality = 2
  )

  expect_is(cm, "list")
  expect_is(cm$complexity_index_x, "data.frame")
  expect_is(cm$complexity_index_y, "data.frame")
  expect_is(cm$balassa_sum_x, "data.frame")
  expect_is(cm$balassa_sum_y, "data.frame")
  expect_equal(nrow(cm$complexity_index_x), 158)
  expect_equal(nrow(cm$complexity_index_y), 991)
  expect_equal(nrow(cm$balassa_sum_x), 158)
  expect_equal(nrow(cm$balassa_sum_y), 991)
})

test_that("complexity_measures fails with NULL iterations", {
  expect_error(
    complexity_measures(
      data = binet_output$balassa_index,
      method = "fitness",
      iterations = NULL,
      extremality = 2
    )
  )
})

test_that("complexity_measures fails with NULL extremality", {
  expect_error(
    complexity_measures(
      data = binet_output$balassa_index,
      method = "fitness",
      iterations = 10,
      extremality = NULL
    )
  )
})
