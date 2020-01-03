## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE-----
knitr::opts_chunk$set(eval = TRUE, message = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
library(binet)

trade_1962

## -----------------------------------------------------------------------------
bi <- balassa_index(
  d = trade_1962,
  x = "country",
  y = "product",
  v = "value"
)

bi

## -----------------------------------------------------------------------------
bi_m <- balassa_index(
  d = trade_1962,
  x = "country",
  y = "product",
  v = "value",
  tbl = F
)

# 5x5 preview
bi_m[1:5,1:5]

## -----------------------------------------------------------------------------
bi_dec <- balassa_index(
  d = trade_1962,
  x = "country",
  y = "product",
  v = "value",
  discrete = F
)

bi_dec

## -----------------------------------------------------------------------------
bi_dec_m <- balassa_index(
  d = trade_1962,
  x = "country",
  y = "product",
  v = "value",
  tbl = F,
  discrete = F
)

# 5x5 preview
bi_dec_m[1:5,1:5]

## -----------------------------------------------------------------------------
com_ref <- complexity_measures(
  bi = bi,
  x = "country",
  y = "product",
  v = "value",
  method = "reflections"
)

com_ref$complexity_index_x
com_ref$complexity_index_y

## -----------------------------------------------------------------------------
com_eig <- complexity_measures(
  bi = bi,
  x = "country",
  y = "product",
  v = "value",
  method = "eigenvalues"
)

com_eig$complexity_index_x
com_eig$complexity_index_y

## -----------------------------------------------------------------------------
com_fit <- complexity_measures(
  bi = bi,
  x = "country",
  y = "product",
  v = "value",
  method = "fitness"
)

com_fit$complexity_index_x
com_fit$complexity_index_y

## -----------------------------------------------------------------------------
pro <- proximity(
  bi = bi,
  x = "country",
  y = "product",
  v = "value",
  d = com_fit$diversity,
  dx = "country",
  dv = "value",
  u = com_fit$ubiquity,
  uy = "product",
  uv = "value"
)

pro$proximity_x
pro$proximity_y

## -----------------------------------------------------------------------------
net <- projections(
  px = pro$proximity_x,
  py = pro$proximity_y,
  tol = 0.1,
  f = "from",
  t = "to",
  v = "value"
)

net$network_x
net$network_y

## ---- fig.width=20, fig.height=12---------------------------------------------
library(igraph)
library(ggraph)
library(magrittr)

set.seed(200100)

net$network_x %>%
  graph_from_data_frame(directed = F) %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = value, edge_width = value),
                 edge_colour = "#a8a8a8") +
  geom_node_point(color = "darkslategray4", size = 8) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("The Country Space") +
  theme_void()

## ---- fig.width=20, fig.height=12---------------------------------------------
net$network_y %>%
  graph_from_data_frame(directed = F) %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = value, edge_width = value),
                 edge_colour = "#a8a8a8") +
  geom_node_point(color = "darkslategray4", size = 4) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("The Product Space") +
  theme_void()

