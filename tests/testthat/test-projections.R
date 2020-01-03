test_that("network results are aligned with the expected output ", {
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
