#' Proximity
#'
#' @export
#'
#' @param bi A matrix or tibble/data.frame. You can pass the
#' output of \code{balassa_index()} here or a similar data structure.
#' @param x string to indicate the column that contains the elements of
#' set X (applies only if d is a tibble/data.frame)
#' @param y string to indicate the column that contains the elements of
#' set Y (applies only if d is a tibble/data.frame)
#' @param v string to indicate the column that contains a metric of the
#' relation between the elements of the sets X and Y (applies only
#' if d is a tibble/data.frame)
#' @param d numeric vector or tibble/data.frame containing diversity
#' measures (e.g. diversity from \code{complexity_measures()})
#' @param u numeric vector or tibble/data.frame containing ubiquity
#' measures (e.g. ubiquity from \code{complexity_measures()})
#' @param dx string to indicate the column that contains
#' the elements of set X in d (default set to NULL)
#' @param dv string to indicate the column that contains
#' a diversity metric for the elements of set X in d (default set to "value")
#' @param uy string to indicate the column that contains
#' the elements of set Y in u (default set to NULL)
#' @param uv string to indicate the column that contains
#' a ubiquity metric for the elements of set Y in u (default set to "value")
#' @param tbl when set to TRUE, the output will be a tibble instead of a
#' matrix (default set to TRUE)
#' @param compute set to "both" by default, it can also be "x" or "y" to
#' obtain one matrix from the bipartite relation
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate pull
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom Matrix Matrix t rowSums colSums
#' @importFrom rlang sym
#'
#' @examples
#' proximity(
#'   bi = binet_output$balassa_index,
#'   x = "country",
#'   y = "product",
#'   v = "value",
#'   d = binet_output$complexity_measures$diversity,
#'   dx = "country",
#'   dv = "value",
#'   u = binet_output$complexity_measures$ubiquity,
#'   uy = "product",
#'   uv = "value"
#' )
#' @references
#' For more information see:
#'
#' \insertRef{atlas2014}{binet}
#'
#' and the references therein.
#'
#' @keywords functions

proximity <- function(bi = NULL,
                      x = "x",
                      y = "y",
                      v = "v",
                      d = NULL,
                      dx = NULL,
                      dv = "value",
                      u = NULL,
                      uy = NULL,
                      uv = "value",
                      tbl = TRUE,
                      compute = "both") {
  # sanity checks ----
  if (all(class(bi) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix",
                               "dgCMatrix") == FALSE)) {
    stop("bi must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (all(class(d) %in% c("numeric", "data.frame") == FALSE) &
    all(class(u) %in% c("numeric", "data.frame") == FALSE)) {
    stop("d and u must be numeric or tibble/data.frame")
  }

  if (!is.logical(tbl)) {
    stop("tbl must be logical")
  }

  if (!any(compute %in% c("both", "x", "y"))) {
    stop("compute must be both, x or y")
  }

  # transformations if bi, d or u are data frames ----
  if (is.data.frame(bi)) {
    bi <- tidyr::spread(bi, !!sym(y), !!sym(v))

    bi_rownames <- dplyr::select(bi, !!sym(x)) %>%
      dplyr::pull()

    bi <- dplyr::select(bi, -!!sym(x)) %>%
      as.matrix()

    bi[is.na(bi)] <- 0

    rownames(bi) <- bi_rownames

    bi <- Matrix::Matrix(bi, sparse = TRUE)
    bi <- bi[Matrix::rowSums(bi) != 0, Matrix::colSums(bi) != 0]
  } else {
    bi <- bi[Matrix::rowSums(bi) != 0, Matrix::colSums(bi) != 0]
  }

  if (is.data.frame(d)) {
    dv <- dplyr::select(d, !!sym(dv)) %>%
      dplyr::pull()

    names(dv) <- dplyr::select(
      d,
      !!sym(dx)
    ) %>%
      dplyr::pull()

    d <- dv
  }

  if (is.data.frame(u)) {
    uv <- dplyr::select(u, !!sym(uv)) %>%
      dplyr::pull()

    names(uv) <- dplyr::select(
      u,
      !!sym(uy)
    ) %>%
      dplyr::pull()

    u <- uv
  }

  # compute proximity matrices ----

  if (compute == "both") {
    compute2 <- c("country", "product")
  } else {
    compute2 <- compute
  }

  if (!is.null(d)) {
    bi <- bi[rownames(bi) %in% names(d), ]
  }

  if (!is.null(u)) {
    bi <- bi[, colnames(bi) %in% names(u)]
  }

  if (any("country" %in% compute2) == TRUE) {
    x1 <- bi %*% Matrix::t(bi)

    x2 <- outer(d, d, pmax)

    if (tbl == FALSE) {
      prox_x <- Matrix::Matrix(x1 / x2, sparse = TRUE)
    } else {
      prox_x <- x1 / x2

      prox_x[upper.tri(prox_x, diag = TRUE)] <- 0

      prox_x <- as.matrix(prox_x) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = rownames(prox_x)) %>%
        tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
        dplyr::filter(!!sym("value") > 0)
    }
  } else {
    prox_x <- NULL
  }

  if (any("product" %in% compute2) == TRUE) {
    p1 <- Matrix::t(bi) %*% bi

    p2 <- outer(u, u, pmax)

    if (tbl == FALSE) {
      prox_y <- Matrix::Matrix(p1 / p2, sparse = TRUE)
    } else {
      prox_y <- p1 / p2

      prox_y[upper.tri(prox_y, diag = TRUE)] <- 0

      prox_y <- as.matrix(prox_y) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = rownames(prox_y)) %>%
        tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
        dplyr::filter(!!sym("value") > 0)
    }
  } else {
    prox_y <- NULL
  }

  return(
    list(
      proximity_x = prox_x,
      proximity_y = prox_y
    )
  )
}
