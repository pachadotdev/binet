ba_ind <- balassa_index(
  d = trade_1962, x = "country", y = "product", v = "value"
)

com_fit <- complexity_measures(
  ba_ind = ba_ind, x = "country", y = "product", v = "value"
)

prox <- proximity(
  ba_ind = ba_ind, x = "country", y = "product", v = "value",
  d = com_fit$diversity, dx = "country", dv = "value",
  u = com_fit$ubiquity, uy = "product", uv = "value",
)

proj <- projections(
  px = pro$proximity_x,
  py = pro$proximity_y,
  v = "value",
  cx = 1,
  cy = 1
)

binet_output <- list(
  balassa_index = ba_ind,
  complexity_measures = com_fit,
  proximity = prox
)

save(binet_output, file = "data/binet_output.rda", compress = "xz")
