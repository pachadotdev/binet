test_that("projection results are aligned with the expected output", {
  # just spanning tree for speed
  net <- projections(
    proximity_source = binet_output$proximity$proximity_source,
    proximity_target = binet_output$proximity$proximity_target,
    tolerance = 1,
    avg_links = 1
  )

  expect_is(net, "list")
  expect_equal(nrow(net$network_source), 157)
  expect_equal(nrow(net$network_target), 990)
  expect_equal(ncol(net$network_source), 3)
  expect_equal(ncol(net$network_target), 3)
})
