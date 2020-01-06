ba_ind <- balassa_index(
  data = trade, source = "country", target = "product", value = "export_value"
)

com_fit <- complexity_measures(ba_ind)

prox <- proximity(
  balassa_index = ba_ind,
  balassa_sum_source = com_fit$balassa_sum_source,
  balassa_sum_target = com_fit$balassa_sum_target
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
