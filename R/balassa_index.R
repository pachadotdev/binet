#' Balassa Index (or Revealed Comparative Advantage, RCA)
#'
#' @export
#'
#' @param d tibble/data.frame in long format or a dense/sparse matrix
#' @param x column with the elements of set X (applies only if d is a
#' tibble/data.frame)
#' @param y column with the elements of set Y (applies only if d is a
#' tibble/data.frame)
#' @param v column with a metric of the relation between the elements of th
#' sets X and Y (applies only if d is a tibble/data.frame)
#' @param discrete when set to TRUE, all the Balassa Index values below
#' the cutoff are converted to zero and zero otherwise (default set to TRUE)
#' @param cutoff reference value used for discretization (default set
#' to 1)
#' @param tbl when set to TRUE, the output will be a tibble instead of a
#' matrix (default set to TRUE)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by ungroup mutate summarise matches
#' pull as_tibble if_else
#' @importFrom tidyr spread gather
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym syms :=
#'
#' @examples
#' balassa_index(
#'   d = trade_1962, x = "country", y = "product", v = "value"
#' )
#'
#' @references
#' For more information see:
#'
#' \insertRef{measuringcomplexity2015}{binet}
#'
#' and the references therein.
#'
#' @keywords functions

balassa_index <- function(d = NULL,
                          x = NULL,
                          y = NULL,
                          v = NULL,
                          cutoff = 1,
                          discrete = TRUE,
                          tbl = TRUE) {
  # sanity checks ----
  if (all(class(d) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix",
                          "dgCMatrix") == FALSE)) {
    stop("d must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (!is.character(x) & !is.character(y) & !is.character(v)) {
    stop("x, y and v must be character")
  }

  if (!is.logical(discrete)) {
    stop("discrete must be TRUE or FALSE")
  }

  if (!is.numeric(cutoff)) {
    stop("cutoff must be numeric")
  }

  if (!is.logical(tbl)) {
    stop("tbl must be matrix or tibble")
  }

  # convert d from matrix to tibble ----
  if (any(class(d) %in% c("dgeMatrix", "dsCMatrix", "dgCMatrix"))) {
    d <- as.matrix(d)
  }

  if (!is.data.frame(d)) {
    d_rownames <- rownames(d)

    d <- as.data.frame(d) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(!!sym(x) := d_rownames) %>%
      tidyr::gather(!!sym(y), !!sym(v), -!!sym(x))
  }

  # aggregate input d by x and y ----
  d <- d %>%
    # Sum by x and y
    dplyr::group_by(!!!syms(c(x, y))) %>%
    dplyr::summarise(vxy = sum(!!sym(v), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!sym("vxy") > 0)

  # compute RCA in tibble form ----
  d <- d %>%
    # Sum by x
    dplyr::group_by(!!sym(x)) %>%
    dplyr::mutate(sum_x_vxy = sum(!!sym("vxy"), na.rm = TRUE)) %>%

    # Sum by y
    dplyr::group_by(!!sym(y)) %>%
    dplyr::mutate(sum_y_vxy = sum(!!sym("vxy"), na.rm = TRUE)) %>%

    # Compute RCA
    dplyr::ungroup() %>%
    dplyr::mutate(
      sum_x_y_vxy = sum(!!sym("vxy"), na.rm = TRUE),
      v = (!!sym("vxy") / !!sym("sum_x_vxy")) / (!!sym("sum_y_vxy") /
                                                   !!sym("sum_x_y_vxy"))
    ) %>%
    dplyr::select(-dplyr::matches("vxy")) %>%

    # Rename columns
    dplyr::rename(x = !!sym(x), y = !!sym(y))

  if (discrete == TRUE) {
    d <- d %>%
      dplyr::mutate(!!sym("v") := dplyr::if_else(!!sym("v") > cutoff, 1, 0))
  }

  if (tbl == FALSE) {
    d <- d %>%
      tidyr::spread(!!sym("y"), !!sym("v"))

    d_rownames <- dplyr::select(d, !!sym("x")) %>%
      dplyr::pull()

    d <- dplyr::select(d, -!!sym("x")) %>% as.matrix()
    d[is.na(d)] <- 0
    rownames(d) <- d_rownames
    d <- Matrix::Matrix(d, sparse = TRUE)
  } else {
    names(d) <- c(x,y,v)
  }

  return(d)
}
