ba_ind <- balassa_index(
  data = trade, source = "country", target = "product", value = "export_value"
)

# ba_ind_ok <- binet_output_ok$balassa_index
#
# all(
#   ba_ind == ba_ind_ok
# )

com_fit <- complexity_measures(ba_ind)

# com_fit_ok <- binet_output_ok$complexity_measures
# names(com_fit_ok$complexity_index_x) <- c("source","value")
# names(com_fit_ok$complexity_index_y) <- c("target","value")
# names(com_fit_ok$diversity) <- c("source","value")
# names(com_fit_ok$ubiquity) <- c("target","value")
#
# all(
#   com_fit$complexity_index_source == com_fit_ok$complexity_index_x
# )
#
# all(
#   com_fit$complexity_index_target == com_fit_ok$complexity_index_y
# )
#
# all(
#   com_fit$balassa_sum_source == com_fit_ok$diversity
# )
#
# all(
#   com_fit$balassa_sum_target == com_fit_ok$ubiquity
# )

prox <- proximity(
  balassa_index = ba_ind,
  balassa_sum_source = com_fit$balassa_sum_source,
  balassa_sum_target = com_fit$balassa_sum_target
)

# prox_ok <- binet_output_ok$proximity
# names(prox_ok$proximity_x) <- c("source", "target", "value")
# names(prox_ok$proximity_y) <- c("source", "target", "value")
#
# all(
#   prox$proximity_source == prox_ok$proximity_x
# )

proj <- projections(
  proximity_source = prox$proximity_source,
  proximity_target = prox$proximity_target,
  tolerance = 0.01,
  avg_links = 4
)

binet_output <- list(
  balassa_index = ba_ind,
  complexity_measures = com_fit,
  proximity = prox
)

save(binet_output, file = "data/binet_output.rda", compress = "xz")
