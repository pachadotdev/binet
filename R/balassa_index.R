#' Balassa Index
#'
#' @description \code{balassa_index()} computes the Balassa Index for a
#' bipartite relation between two disjoint sets X, the "source" or "from" side,
#' and Y, the "target" or "to" side.
#'
#' @details The current implementation follows
#' \insertCite{measuringcomplexity2015}{binet} to obtain a metric for
#' specialisation. In the context of international trade, if the Balassa Index
#' for a country-product pair is more than 1, it means that country is
#' specialized in that product.
#'
#' @return A data.frame with the Balassa Index.
#'
#' @param data (Type: data.frame) a demo datasets such as \code{trade}
#' or \code{omim} from this package or any arrangement with a bipartite
#' relation.
#' @param source (Type: character) the column with the elements of set X.
#' By default this is set to \code{"source"}.
#' @param target (Type: character) the column with the elements of set Y.
#' By default this is set to \code{"target"}.
#' @param value (Type: character) the column with the binary expression for the
#' Balassa Index.
#' By default this is set to \code{"value"}.
#' @param discrete (Type: logical) whether converting the Balassa Index to
#' discrete (0/1) values. Anything below the specified cutoff is converted to 0
#' and 1 otherwise. By default this is set to \code{TRUE}.
#' @param cutoff (Type: numeric) the cutoff to use for discretization.
#' By default this is set to \code{1}.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by ungroup mutate summarise matches
#' if_else
#' @importFrom rlang sym syms
#'
#' @examples
#' balassa_index(
#'     data = galactic_federation,
#'     source = "planet",
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
  if (!any(class(data) %in% "data.frame")) {
    stop("'data' must be a data.frame")
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

  # Aggregate input by X and Y ----
  data <- data %>%
    # Select and transform inputs
    dplyr::select(!!!syms(c(source, target, value))) %>%
    dplyr::mutate(
      source = as.character(!!sym(source)),
      target = as.character(!!sym(target)),
      value = as.numeric(!!sym(value))
    ) %>%

    # Sum by X and Y
    dplyr::group_by(!!!syms(c("source", "target"))) %>%
    dplyr::summarise(vxy = sum(!!sym("value"), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!sym("vxy") > 0)

  # Compute Balassa Index ----
  data <- data %>%
    # Sum by X
    dplyr::group_by(!!sym("source")) %>%
    dplyr::mutate(sum_x_vxy = sum(!!sym("vxy"), na.rm = TRUE)) %>%

    # Sum by Y
    dplyr::group_by(!!sym("target")) %>%
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
      dplyr::mutate(value = dplyr::if_else(!!sym("value") > cutoff, 1, 0))
  }

  return(data)
}
