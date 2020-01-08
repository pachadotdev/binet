#' Proximity
#'
#' @description TBDs
#'
#' @details TBD
#'
#' @param balassa_index a data frame (e.g. the output from
#' \code{balassa_index()}).
#' @param balassa_sum_source numeric vector or tibble/data.frame containing the
#' Balassa sum for elements of set X (e.g. \code{balassa_sum_source} from
#' \code{complexity_measures()}).
#' @param balassa_sum_target numeric vector or tibble/data.frame containing the
#' Balassa sum for elements of set Y (e.g. \code{balassa_sum_target} from
#' \code{complexity_measures()}).
#' @param source a column with the elements of set X (applies only if data is
#' a data frame).
#' @param target a column with the elements of set Y (applies only if data is
#' a data frame).
#' @param value a column with some metric of the relation between the elements
#' of X and Y (applies only if data is a data frame).
#' @param compute which proximity to compute. By default is "both" (both
#' proximities) but it can also be "source" or "target".
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble select filter mutate pull
#' @importFrom tibble deframe
#' @importFrom tidyr spread gather
#' @importFrom Matrix Matrix t rowSums colSums
#' @importFrom rlang sym syms :=
#'
#' @examples
#' proximity(
#'   balassa_index = binet_output$balassa_index,
#'   balassa_sum_source = binet_output$complexity_measures$balassa_sum_source,
#'   balassa_sum_target = binet_output$complexity_measures$balassa_sum_target
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

proximity <- function(balassa_index, balassa_sum_source, balassa_sum_target,
                      source = "source", target = "target", value = "value", compute = "both") {
  # sanity checks ----
  if (!any(class(balassa_index) %in% "data.frame")) {
    stop("'balassa_index' must be a data.frame")
  }

  if (all(class(balassa_sum_source) %in% c("numeric", "data.frame") == FALSE) |
    all(class(balassa_sum_target) %in% c("numeric", "data.frame") == FALSE)) {
    stop("'balassa_sum_source' and 'balassa_sum_target' must be data.frame or numeric")
  }

  if (is.numeric(balassa_sum_source) & is.null(names(balassa_sum_source))) {
    stop("'balassa_sum_source' cannot have NULL names")
  }

  if (is.numeric(balassa_sum_target) & is.null(names(balassa_sum_target))) {
    stop("'balassa_sum_target' cannot have NULL names")
  }

  if (!any(compute %in% c("both", "source", "target"))) {
    stop("'compute' must be 'both', 'source' or 'target'")
  }

  # transformations for data frame inputs ----
  balassa_index <- dplyr::select(balassa_index, !!!syms(c(source, target, value))) %>%
    tidyr::spread(!!sym(target), !!sym(value))

  balassa_index_rownames <- dplyr::select(balassa_index, !!sym(source)) %>%
    dplyr::pull()

  balassa_index <- dplyr::select(balassa_index, -!!sym(source)) %>%
    as.matrix()

  balassa_index[is.na(balassa_index)] <- 0

  rownames(balassa_index) <- balassa_index_rownames

  balassa_index <- Matrix::Matrix(balassa_index, sparse = TRUE)
  balassa_index <- balassa_index[Matrix::rowSums(balassa_index) != 0, Matrix::colSums(balassa_index) != 0]

  balassa_sum_source <- tibble::deframe(balassa_sum_source)

  balassa_sum_target <- tibble::deframe(balassa_sum_target)

  # compute proximity matrices ----

  if (compute == "both") {
    compute2 <- c("source", "target")
  } else {
    compute2 <- compute
  }

  if (!is.null(balassa_sum_source)) {
    balassa_index <- balassa_index[rownames(balassa_index) %in% names(balassa_sum_source), ]
  }

  if (!is.null(balassa_sum_target)) {
    balassa_index <- balassa_index[, colnames(balassa_index) %in% names(balassa_sum_target)]
  }

  if (any("source" %in% compute2) == TRUE) {
    p1 <- balassa_index %*% Matrix::t(balassa_index)

    p2 <- outer(balassa_sum_source, balassa_sum_source, pmax)

    prox_x <- p1 / p2

    prox_x[upper.tri(prox_x, diag = TRUE)] <- 0

    prox_x <- as.matrix(prox_x) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(!!sym("source") := rownames(prox_x)) %>%
      tidyr::gather(!!sym("target"), !!sym("value"), -!!sym("source")) %>%
      dplyr::filter(!!sym("value") > 0)
  } else {
    prox_x <- NULL
  }

  if (any("target" %in% compute2) == TRUE) {
    p1 <- Matrix::t(balassa_index) %*% balassa_index

    p2 <- outer(balassa_sum_target, balassa_sum_target, pmax)

    prox_y <- p1 / p2

    prox_y[upper.tri(prox_y, diag = TRUE)] <- 0

    prox_y <- as.matrix(prox_y) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(!!sym("source") := rownames(prox_y)) %>%
      tidyr::gather(!!sym("target"), !!sym("value"), -!!sym("source")) %>%
      dplyr::filter(!!sym("value") > 0)
  } else {
    prox_y <- NULL
  }

  return(
    list(
      proximity_source = prox_x,
      proximity_target = prox_y
    )
  )
}
