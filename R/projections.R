#' Projections of a bipartite network
#'
#' @export
#'
#' @param px tibble/data.frame or sparse/dense matrix containing proximity
#' measures for the elements of set X (e.g. proximity_x from \code{proximity()})
#' @param py tibble/data.frame or sparse/dense matrix containing proximity
#' measures for the elements of set Y (e.g. proximity_y from \code{proximity()})
#' @param f column with the "from" side
#' in a relation between elements of the same set (applies only if d is a
#' tibble/data.frame)
#' @param t column with the "to" side in a relation between elements of the same
#' set (applies only if d is a tibble/data.frame)
#' @param v column with a metric of the relation between the elements of the
#' same set (applies only if d is a tibble/data.frame)
#' @param ax average number of connections for the projection of X
#' (default set to 4)
#' @param ay average number of connections for the projection of Y
#' (default set to 4)
#' @param tol tolerance for proximity variation on each iteration until
#' obtaining the desired average number of connections (default set to 0.01)
#' @param tbl when set to TRUE the output will be a tibble instead of a
#' graph (set to TRUE by default)
#' @param compute set to "both" by default, it can also be "x" or "y" to
#' obtain one projection of the bipartite network
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble filter mutate bind_rows
#' @importFrom tidyr gather
#' @importFrom igraph graph_from_data_frame mst as_data_frame simplify degree
#' @importFrom rlang sym
#'
#' @examples
#' projections(
#'   px = binet_output$proximity$proximity_x,
#'   py = binet_output$proximity$proximity_y,
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

projections <- function(px = NULL,
                        py = NULL,
                        f = "from",
                        t = "to",
                        v = "value",
                        ax = 4,
                        ay = 4,
                        tol = 0.01,
                        tbl = TRUE,
                        compute = "both") {
  # sanity checks ----
  if (all(class(px) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix",
                           "dsCMatrix") == FALSE) &
    all(class(py) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix",
                         "dsCMatrix") == FALSE)) {
    stop("px and py must be tibble/data.frame or dense/sparse matrix")
  }

  if (!is.numeric(ax) & !is.numeric(ay)) {
    stop("ax & ay must be numeric")
  }

  if (!is.logical(tbl)) {
    stop("tbl must be matrix or tibble")
  }

  if (!any(compute %in% c("both", "country", "product"))) {
    stop("compute must be both, country or product")
  }

  if (compute == "both") {
    compute2 <- c("country", "product")
  } else {
    compute2 <- compute
  }

  if (any("country" %in% compute2) == TRUE) {
    # arrange country matrix ----
    if (any(class(px) %in% c("dgeMatrix", "dgCMatrix", "dsCMatrix") == TRUE)) {
      px <- as.matrix(px)
    }

    if (is.matrix(px)) {
      px[upper.tri(px, diag = TRUE)] <- 0
      row_names <- rownames(px)

      px <- px %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = row_names) %>%
        tidyr::gather(!!sym(t), !!sym(v), -!!sym(f)) %>%
        dplyr::filter(!!sym(v) > 0)
    }

    # compute countries network ----
    px <- dplyr::mutate(px,
      value = -1 * !!sym(v)
    )

    message("Computing projection of X...")

    xg <- igraph::graph_from_data_frame(px, directed = FALSE)

    x_mst <- igraph::mst(xg,
      weights = px$value,
      algorithm = "prim"
    )
    x_mst <- igraph::as_data_frame(x_mst)

    threshold <- 0
    avg_links_n <- FALSE

    while(avg_links_n == FALSE) {
      if (threshold < -1) {
        message("no threshold achieves the avg number of connections for X projection, returning maximum spanning tree")
        xg <- dplyr::mutate(x_mst, value = -1 * !!sym(v))
        xg <- igraph::graph_from_data_frame(xg, directed = FALSE)
        avg_links_n <- TRUE
      } else {
        message(sprintf("%s threshold...", -1 * threshold))

        xg_nmst <- px %>%
          dplyr::filter(!!sym(v) <= -1 * threshold) %>%
          dplyr::anti_join(x_mst, by = c("from", "to"))

        xg <- dplyr::bind_rows(x_mst, xg_nmst)
        xg <- dplyr::mutate(xg, value = -1 * !!sym(v))

        xg <- igraph::graph_from_data_frame(xg, directed = FALSE)
        xg <- igraph::simplify(xg,
                               remove.multiple = TRUE, remove.loops = TRUE,
                               edge.attr.comb = "first"
        )

        avg_links <- mean(igraph::degree(xg))
        avg_links_n <- ifelse(avg_links <= 4, TRUE, FALSE)
        threshold <- threshold - tol

        if (avg_links_n) {
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

  if (any("product" %in% compute2) == TRUE) {
    # arrange products matrix ----
    if (any(class(py) %in% c("dgeMatrix", "dgCMatrix", "dsCMatrix") == TRUE)) {
      py <- as.matrix(py)
    }

    if (is.matrix(py)) {
      py[upper.tri(py, diag = TRUE)] <- 0
      row_names <- rownames(py)

      py <- py %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = row_names) %>%
        tidyr::gather(!!sym(t), !!sym(v), -!!sym(f)) %>%
        dplyr::filter(!!sym(v) > 0)
    }

    # compute products network ----
    py <- dplyr::mutate(py,
      value = -1 * !!sym(v)
    )

    message("Computing Y projection...")

    yg <- igraph::graph_from_data_frame(py, directed = FALSE)

    y_mst <- igraph::mst(yg,
      weights = py$value,
      algorithm = "prim"
    )
    y_mst <- igraph::as_data_frame(y_mst)

    threshold <- 0
    avg_links_n <- FALSE

    while(avg_links_n == FALSE) {
      if (threshold < -1) {
        message("no threshold achieves the avg number of connections for Y projection, returning maximum spanning tree")
        yg <- dplyr::mutate(y_mst, value = -1 * !!sym(v))
        yg <- igraph::graph_from_data_frame(yg, directed = FALSE)
        avg_links_n <- TRUE
      } else {
        message(sprintf("%s threshold...", -1 * threshold))

        yg_nmst <- py %>%
          dplyr::filter(!!sym(v) <= -1 * threshold) %>%
          dplyr::anti_join(y_mst, by = c("from", "to"))

        yg <- dplyr::bind_rows(y_mst, yg_nmst)
        yg <- dplyr::mutate(yg, value = -1 * !!sym(v))

        yg <- igraph::graph_from_data_frame(yg, directed = FALSE)
        yg <- igraph::simplify(yg,
                               remove.multiple = TRUE, remove.loops = TRUE,
                               edge.attr.comb = "first"
        )

        avg_links <- mean(igraph::degree(yg))
        avg_links_n <- ifelse(avg_links <= 4, TRUE, FALSE)
        threshold <- threshold - tol

        if (avg_links_n) {
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
