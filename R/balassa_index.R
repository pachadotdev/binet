#' Balassa Index
#'
#' @description \code{balassa_index()} computes the Balassa Index for a
#' bipartite relation between two disjoint sets X (the "source" side) and Y (the
#' "target" side).
#'
#' @details TBD
#'
#' @param data a data frame or matrix.
#' @param source a column with the elements of set X (applies only if data is
#' a data frame).
#' @param target a column with the elements of set Y (applies only if data is
#' a data frame).
#' @param value a column with some metric of the relation between the elements
#' of X and Y (applies only if data is a data frame).
#' @param discrete if \code{TRUE} the Balassa Index values are converted to
#' discrete values (1/0) unless you set this to \code{FALSE}. Anything below the
#' specified cutoff is converted to 0 and 1 otherwise.
#' @param cutoff the cutoff value used for discretization. The default is 1 but
#' it can be any numeric value.
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
#'     data = trade,
#'     source = "country",
#'     target = "product",
#'     value = "export_value"
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
#'
#' @export

balassa_index <- function(data, source = "source", target = "target", value = "value",
                          discrete = TRUE, cutoff = 1) {
  # sanity checks ----
  if (all(class(data) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix",
                          "dgCMatrix") == FALSE)) {
    stop("'data' must be a data.frame or matrix")
  }

  if (!is.character(source) | !is.character(target) | !is.character(value)) {
    stop("'source', 'target' and 'value' must be of type character")
  }

  if (!is.logical(discrete)) {
    stop("'discrete' must be TRUE or FALSE")
  }

  if (!is.numeric(cutoff)) {
    stop("'cutoff' must be numeric")
  }

  # convert matrix to data.frame ----
  if (any(class(data) %in% c("dgeMatrix", "dsCMatrix", "dgCMatrix"))) {
    data <- as.matrix(data)
  }

  if (is.matrix(data)) {
    data_rownames <- rownames(data)

    data <- as.data.frame(data) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(!!sym(source) := data_rownames) %>%
      tidyr::gather(!!sym(target), !!sym(value), -!!sym(source))
  }

  # aggregate input by x and y ----
  data <- data %>%
    # Sum by x and y
    dplyr::group_by(!!!syms(c(source, target))) %>%
    dplyr::summarise(vxy = sum(!!sym(value), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!sym("vxy") > 0)

  # compute RCA in tibble form ----
  data <- data %>%
    # Sum by x
    dplyr::group_by(!!sym(source)) %>%
    dplyr::mutate(sum_x_vxy = sum(!!sym("vxy"), na.rm = TRUE)) %>%

    # Sum by y
    dplyr::group_by(!!sym(target)) %>%
    dplyr::mutate(sum_y_vxy = sum(!!sym("vxy"), na.rm = TRUE)) %>%

    # Compute BI
    dplyr::ungroup() %>%
    dplyr::mutate(
      sum_x_y_vxy = sum(!!sym("vxy"), na.rm = TRUE),
      value = (!!sym("vxy") / !!sym("sum_x_vxy")) / (!!sym("sum_y_vxy") / !!sym("sum_x_y_vxy"))
    ) %>%
    dplyr::select(-dplyr::matches("vxy"))

  if (discrete == TRUE) {
    data <- data %>%
      dplyr::mutate(!!sym("value") := dplyr::if_else(!!sym("value") > cutoff, 1, 0))
  }

  names(data) <- c("source", "target", "value")

  return(data)
}
