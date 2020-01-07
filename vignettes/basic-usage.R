## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE-----
knitr::opts_chunk$set(eval = TRUE, message = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
library(binet)

trade

## -----------------------------------------------------------------------------
bi <- balassa_index(
  data = trade,
  source = "country",
  target = "product",
  value = "export_value"
)

bi

## -----------------------------------------------------------------------------
bi_dec <- balassa_index(
  data =trade,
  source = "country",
  target = "product",
  value = "export_value",
  discrete = F
)

bi_dec

## -----------------------------------------------------------------------------
com_fit <- complexity_measures(balassa_index = bi)

com_fit$complexity_index_source
com_fit$complexity_index_target

## -----------------------------------------------------------------------------
com_ref <- complexity_measures(
  balassa_index = bi,
  method = "reflections"
)

com_ref$complexity_index_source
com_ref$complexity_index_target

## -----------------------------------------------------------------------------
com_eig <- complexity_measures(
  balassa_index = bi,
  method = "eigenvalues"
)

com_eig$complexity_index_source
com_eig$complexity_index_target

## -----------------------------------------------------------------------------
pro <- proximity(
  balassa_index = bi,
  balassa_sum_source = com_fit$balassa_sum_source,
  balassa_sum_target = com_fit$balassa_sum_target
)

pro$proximity_source
pro$proximity_target

## -----------------------------------------------------------------------------
net <- projections(
  proximity_source = pro$proximity_source,
  proximity_target = pro$proximity_target,
  tolerance = 0.05
)

net$network_source
net$network_target

## ---- fig.width=20, fig.height=12---------------------------------------------
library(igraph)
library(ggraph)
library(magrittr)

set.seed(200100)

net$network_source %>%
  graph_from_data_frame(directed = F) %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = value, edge_width = value),
                 edge_colour = "#a8a8a8") +
  geom_node_point(color = "darkslategray4", size = 8) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("The Country Space") +
  theme_void()

## ---- fig.width=20, fig.height=12---------------------------------------------
net$network_target %>%
  graph_from_data_frame(directed = F) %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = value, edge_width = value),
                 edge_colour = "#a8a8a8") +
  geom_node_point(color = "darkslategray4", size = 4) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("The Product Space") +
  theme_void()

