#' Projections of a bipartite network
#'
#' @description TBD
#'
#' @details TBD
#'
#' @param proximity_source a data frame containing proximity
#' values for the elements of set X (e.g. proximity_source from \code{proximity()})
#' @param proximity_target a data frame containing proximity
#' values for the elements of set Y (e.g. proximity_target from \code{proximity()})
#' @param source a column with the elements of set X (applies only if data is
#' a data frame).
#' @param target a column with the elements of set Y (applies only if data is
#' a data frame).
#' @param value a column with the proximity values between the elements
#' of X or Y (applies only if data is a data frame).
#' @param avg_links average number of connections for the projection of X
#' (default set to 4)
#' @param tolerance tolerance for proximity variation on each iteration until
#' obtaining the desired average number of connections (default set to 0.05)
#' @param compute which projection to compute. By default is "both" (both
#' projections) but it can also be "source" or "target".
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble filter mutate mutate_if bind_rows
#' @importFrom igraph graph_from_data_frame mst as_data_frame simplify degree
#' @importFrom rlang sym
#'
#' @examples
#' projections(
#'   proximity_source = binet_output$proximity$proximity_source,
#'   proximity_target = binet_output$proximity$proximity_target
#' )
#'
#' @references
#' For more information see:
#'
#' \insertRef{atlas2014}{binet}
#'
#' and the references therein.
#'
#' @keywords functions
#'
#' @export

projections <- function(proximity_source, proximity_target,
                        source = "source", target = "target", value = "value",
                        avg_links = 4, tolerance = 0.05, compute = "both") {
  # sanity checks ----
  if (!any(class(proximity_source) %in% "data.frame") |
      !any(class(proximity_target) %in% "data.frame")) {
    stop("'proximity_source' and 'proximity_target' must be data.frame")
  }

  if (!is.numeric(avg_links)) {
    stop("'avg_links' must be numeric")
  }

  if (!any(compute %in% c("both", "source", "target"))) {
    stop("'compute' must be 'both', 'source' or 'target'")
  }

  if (compute == "both") {
    compute2 <- c("source", "target")
  } else {
    compute2 <- compute
  }

  trim_network <- function(proximity_d, avg_d) {
    # compute network ----
    proximity_d <- proximity_d %>%
      dplyr::mutate(value = -1 * !!sym(value)) %>%
      dplyr::mutate_if(is.factor, as.character)

    proximity_g <- igraph::graph_from_data_frame(proximity_d, directed = FALSE)

    proximity_mst <- igraph::mst(proximity_g,
                                 weights = proximity_d$value,
                                 algorithm = "prim"
    )

    proximity_mst <- igraph::as_data_frame(proximity_mst)
    names(proximity_mst) <- c(source, target, value)

    threshold <- 0
    avg_links_n <- FALSE

    while(avg_links_n == FALSE) {
      if (threshold <= -1) {
        warning("no threshold achieves the avg number of connections\nreturning maximum spanning tree")
        proximity_g <- dplyr::mutate(proximity_mst, value = -1 * !!sym(value))
        proximity_g <- igraph::graph_from_data_frame(proximity_g, directed = FALSE)
        avg_links_n <- TRUE

        return(proximity_g)
      } else {
        message(sprintf("%s threshold...", -1 * threshold))

        proximity_g_nmst <- proximity_d %>%
          dplyr::filter(!!sym(value) <= threshold) %>%
          dplyr::anti_join(proximity_mst, by = c("source", "target"))

        proximity_g <- dplyr::bind_rows(proximity_mst, proximity_g_nmst)
        proximity_g <- dplyr::mutate(proximity_g, value = -1 * !!sym(value))

        proximity_g <- igraph::graph_from_data_frame(proximity_g, directed = FALSE)

        proximity_g <- igraph::simplify(proximity_g,
                                        remove.multiple = TRUE, remove.loops = TRUE,
                                        edge.attr.comb = "first"
        )

        avg_links <- mean(igraph::degree(proximity_g))
        avg_links_n <- ifelse(avg_links <= avg_d, TRUE, FALSE)
        threshold <- threshold - tolerance

        if (avg_links_n == TRUE) {
          message(sprintf("%s threshold achieves the avg number of connections", -1 * threshold))
          return(proximity_g)
        }
      }
    }
  }

  if (any("source" %in% compute2) == TRUE) {
    message("computing target projection...")
    message(rep("-", 50))
    xg <- trim_network(proximity_source, avg_links)
    xg <- igraph::as_data_frame(xg) %>%
      dplyr::as_tibble()
  } else {
    xg <- NULL
  }

  if (any("target" %in% compute2) == TRUE) {
    message("computing target projection...")
    message(rep("-", 50))
    yg <- trim_network(proximity_target, avg_links)
    yg <- igraph::as_data_frame(yg) %>%
      dplyr::as_tibble()
  } else {
    yg <- NULL
  }

  return(
    list(
      network_source = xg,
      network_target = yg
    )
  )
}
