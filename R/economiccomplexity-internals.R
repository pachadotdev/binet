#' Source-target aggregation
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by summarise ungroup filter mutate
#' @importFrom rlang sym syms :=
#' @keywords internal
source_target_aggregation <- function(dataframe, source = "source", target = "target", value = "value") {
  return(
    dataframe %>%
      select(!!!syms(c(source, target, value))) %>%
      group_by(!!!syms(c(source, target))) %>%
      summarise(!!sym(value) := sum(!!sym(value), na.rm = TRUE)) %>%
      ungroup() %>%
      filter(!!sym("value") > 0) %>%
      mutate(
        source = as.factor(!!sym(source)),
        target = as.factor(!!sym(target)),
        value = as.numeric(!!sym(value))
      )
  )
}

#' Dataframe to matrix
#' @importFrom Matrix sparseMatrix
#' @keywords internal
dataframe_to_matrix <- function(dataframe, source = "source", target = "target", value = "value") {
  return(
    with(
      dataframe,
      sparseMatrix(
        i = as.numeric(source),
        j = as.numeric(target),
        x = value,
        dimnames = list(levels(source), levels(target))
      )
    )
  )
}
