#' Projections of a bipartite network
#'
#' @export
#'
#' @param proximity_source tibble/data.frame or sparse/dense matrix containing proximity
#' measures for the elements of set X (e.g. proximity_x from \code{proximity()})
#' @param proximity_target tibble/data.frame or sparse/dense matrix containing proximity
#' measures for the elements of set Y (e.g. proximity_y from \code{proximity()})
#' @param f column with the "from" side
#' in a relation between elements of the same set (applies only if d is a
#' tibble/data.frame)
#' @param t column with the "to" side in a relation between elements of the same
#' set (applies only if d is a tibble/data.frame)
#' @param v column with a metric of the relation between the elements of the
#' same set (applies only if d is a tibble/data.frame)
#' @param avg_source average number of connections for the projection of X
#' (default set to 4)
#' @param avg_target average number of connections for the projection of Y
#' (default set to 4)
#' @param tol tolerance for proximity variation on each iteration until
#' obtaining the desired average number of connections (default set to 0.01)
#' @param tbl when set to TRUE the output will be a tibble instead of a
#' graph (set to TRUE by default)
#' @param pro set to compute "both" projections by default, it can also be
#' "x" or "y" to obtain one projection of the bipartite network
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble filter mutate bind_rows
#' @importFrom tidyr gather
#' @importFrom igraph graph_from_data_frame mst as_data_frame simplify degree
#' @importFrom rlang sym
#'
#' @examples
#' projections(
#'   proximity_source = binet_output$proximity$proximity_x,
#'   proximity_target = binet_output$proximity$proximity_y,
#'   f = "from",
#'   t = "to",
#'   v = "value",
#'   tol = 0.25,
#'   tbl = TRUE
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

projections <- function(proximity_source = NULL,
                        proximity_target = NULL,
                        avg_source = 4,
                        avg_target = 4,
                        tol = 0.01,
                        pro = "both") {
  # sanity checks ----
  if (all(class(proximity_source) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix",
                           "dsCMatrix") == FALSE) |
    all(class(proximity_target) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix",
                         "dsCMatrix") == FALSE)) {
    stop("proximity_source and proximity_target must be tibble/data.frame or dense/sparse matrix")
  }

  if (!is.numeric(avg_source) | !is.numeric(avg_target)) {
    stop("avg_source and avg_target must be numeric")
  }

  if (!is.logical(tbl)) {
    stop("tbl must be matrix or tibble")
  }

  if (!any(pro %in% c("both", "x", "y"))) {
    stop("projections must be both, x or y")
  }

  if (pro == "both") {
    pro2 <- c("x", "y")
  } else {
    pro2 <- pro
  }

  if (any("x" %in% pro2) == TRUE) {
    # arrange x matrix ----
    if (any(class(proximity_source) %in% c("dgeMatrix", "dgCMatrix", "dsCMatrix") == TRUE)) {
      proximity_source <- as.matrix(proximity_source)
    }

    if (is.matrix(proximity_source)) {
      proximity_source[upper.tri(proximity_source, diag = TRUE)] <- 0
      row_names <- rownames(proximity_source)

      proximity_source <- proximity_source %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = row_names) %>%
        tidyr::gather(!!sym("target"), !!sym("value"), -!!sym("source")) %>%
        dplyr::filter(!!sym("value") > 0)
    }

    # compute x network ----
    proximity_source <- dplyr::mutate(proximity_source,
      value = -1 * !!sym("value")
    )

    message("Computing projection of X...")

    xg <- igraph::graph_from_data_frame(proximity_source, directed = FALSE)

    x_mst <- igraph::mst(xg,
      weights = proximity_source$value,
      algorithm = "prim"
    )
    x_mst <- igraph::as_data_frame(x_mst)

    threshold <- 0
    avg_links_n <- FALSE

    while(avg_links_n == FALSE) {
      if (threshold <= -1) {
        message("no threshold achieves the avg number of connections for X projection, returning maximum spanning tree")
        xg <- dplyr::mutate(x_mst, value = -1 * !!sym("value"))
        xg <- igraph::graph_from_data_frame(xg, directed = FALSE)
        avg_links_n <- TRUE
      } else {
        message(sprintf("%s threshold...", threshold))

        xg_nmst <- proximity_source %>%
          dplyr::filter(!!sym("value") <= threshold) %>%
          dplyr::anti_join(x_mst, by = c("from", "to"))

        xg <- dplyr::bind_rows(x_mst, xg_nmst)
        xg <- dplyr::mutate(xg, value = -1 * !!sym("value"))

        xg <- igraph::graph_from_data_frame(xg, directed = FALSE)
        xg <- igraph::simplify(xg,
                               remove.multiple = TRUE, remove.loops = TRUE,
                               edge.attr.comb = "first"
        )

        avg_links <- mean(igraph::degree(xg))
        avg_links_n <- ifelse(avg_links <= avg_source, TRUE, FALSE)
        threshold <- threshold - tol

        if (avg_links_n == TRUE) {
          message(sprintf("%s threshold achieves the avg number of connections for X projection", -1 * threshold))
        }
      }
    }

    if (tbl == TRUE) {
      xg <- igraph::as_data_frame(xg) %>% dplyr::as_tibble()
    }
  } else {
    xg <- NULL
  }

  if (any("y" %in% pro2) == TRUE) {
    # arrange y matrix ----
    if (any(class(proximity_target) %in% c("dgeMatrix", "dgCMatrix", "dsCMatrix") == TRUE)) {
      proximity_target <- as.matrix(proximity_target)
    }

    if (is.matrix(proximity_target)) {
      proximity_target[upper.tri(proximity_target, diag = TRUE)] <- 0
      row_names <- rownames(proximity_target)

      proximity_target <- proximity_target %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = row_names) %>%
        tidyr::gather(!!sym("target"), !!sym("value"), -!!sym("source")) %>%
        dplyr::filter(!!sym("value") > 0)
    }

    # compute y network ----
    proximity_target <- dplyr::mutate(proximity_target,
      value = -1 * !!sym("value")
    )

    message("Computing Y projection...")

    yg <- igraph::graph_from_data_frame(proximity_target, directed = FALSE)

    y_mst <- igraph::mst(yg,
      weights = proximity_target$value,
      algorithm = "prim"
    )
    y_mst <- igraph::as_data_frame(y_mst)

    threshold <- 0
    avg_links_n <- FALSE

    while(avg_links_n == FALSE) {
      if (threshold <= -1) {
        message("no threshold achieves the avg number of connections for Y projection, returning maximum spanning tree")
        yg <- dplyr::mutate(y_mst, value = -1 * !!sym("value"))
        yg <- igraph::graph_from_data_frame(yg, directed = FALSE)
        avg_links_n <- TRUE
      } else {
        message(sprintf("%s threshold...", threshold))

        yg_nmst <- proximity_target %>%
          dplyr::filter(!!sym("value") <= threshold) %>%
          dplyr::anti_join(y_mst, by = c("from", "to"))

        yg <- dplyr::bind_rows(y_mst, yg_nmst)
        yg <- dplyr::mutate(yg, value = -1 * !!sym("value"))

        yg <- igraph::graph_from_data_frame(yg, directed = FALSE)
        yg <- igraph::simplify(yg,
                               remove.multiple = TRUE, remove.loops = TRUE,
                               edge.attr.comb = "first"
        )

        avg_links <- mean(igraph::degree(yg))
        avg_links_n <- ifelse(avg_links <= avg_target, TRUE, FALSE)
        threshold <- threshold - tol

        if (avg_links_n == TRUE) {
          message(sprintf("%s threshold achieves the avg number of connections for Y projection", -1 * threshold))
        }
      }
    }

    if (tbl == TRUE) {
      yg <- igraph::as_data_frame(yg) %>% dplyr::as_tibble()
    }
  } else {
    yg <- NULL
  }

  return(
    list(
      network_x = xg,
      network_y = yg
    )
  )
}
