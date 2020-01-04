test_that("network results are aligned with the expected output", {
  net <- projections(
    px = binet_output$proximity$proximity_x,
    py = binet_output$proximity$proximity_y,
    tol = 1,
    tbl = T
  )

  expect_is(net, "list")
  expect_equal(nrow(net$network_x), 157)
  expect_equal(nrow(net$network_y), 990)
  expect_equal(ncol(net$network_x), 3)
  expect_equal(ncol(net$network_y), 3)
})

test_that("network results work with matrix input", {
  pr <- proximity(
    bi = binet_output$balassa_index,
    x = "country",
    y = "product",
    v = "value",
    d = binet_output$complexity_measures$diversity,
    dx = "country",
    u = binet_output$complexity_measures$ubiquity,
    uy = "product",
    tbl = F
  )

  net <- projections(
    px = pr$proximity_x,
    py = pr$proximity_y,
    tol = 1,
    tbl = F
  )

  expect_is(net, "list")
  expect_is(net$network_x, "igraph")
  expect_is(net$network_y, "igraph")
})

test_that("projections fail with a null proximity", {
  expect_error(
    projections(
      px = NULL,
      py = binet_output$proximity$proximity_y,
      tol = 1,
      tbl = T
    )
  )
})

test_that("projections fail with null average nodes", {
  expect_error(
    projections(
      px = binet_output$proximity$proximity_x,
      py = binet_output$proximity$proximity_y,
      ax = NULL,
      tol = 1,
      tbl = T
    )
  )
})

test_that("projections fail with a character tibble", {
  expect_error(
    projections(
      px = binet_output$proximity$proximity_x,
      py = binet_output$proximity$proximity_y,
      ax = 4,
      tol = 1,
      tbl = "yes"
    )
  )
})

test_that("projections returns x projection only", {
  net <- projections(
      px = binet_output$proximity$proximity_x,
      py = binet_output$proximity$proximity_y,
      ax = 4,
      tol = 1,
      tbl = T,
      pro = "x"
  )

  expect_is(net, "list")
  expect_equal(nrow(net$network_x), 157)
  expect_equal(ncol(net$network_x), 3)
})

test_that("projections returns x projection only and returns not just spanning tree", {
  net <- projections(
    px = binet_output$proximity$proximity_x,
    py = binet_output$proximity$proximity_y,
    ax = 4,
    tol = 0.1,
    tbl = T,
    pro = "x"
  )

  expect_is(net, "list")
  expect_equal(nrow(net$network_x), 269)
  expect_equal(ncol(net$network_x), 3)
})

test_that("projections returns y projection only", {
  net <- projections(
    px = binet_output$proximity$proximity_x,
    py = binet_output$proximity$proximity_y,
    ay = 4,
    tol = 1,
    tbl = T,
    pro = "y"
  )

  expect_is(net, "list")
  expect_equal(nrow(net$network_y), 990)
  expect_equal(ncol(net$network_y), 3)
})

test_that("projections returns y projection only and returns not just spanning tree", {
  net <- projections(
    px = binet_output$proximity$proximity_x,
    py = binet_output$proximity$proximity_y,
    ay = 4,
    tol = 0.1,
    tbl = T,
    pro = "y"
  )

  expect_is(net, "list")
  expect_equal(nrow(net$network_y), 1329)
  expect_equal(ncol(net$network_y), 3)
})

test_that("projections fail with a NULL projection", {
  expect_error(
    projections(
      px = binet_output$proximity$proximity_x,
      py = binet_output$proximity$proximity_y,
      ax = 4,
      tol = 1,
      tbl = T,
      pro = NULL
    )
  )
})
