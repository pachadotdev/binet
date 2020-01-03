#' Complexity Measures
#'
#' @export
#'
#' @param bi A matrix or tibble/data.frame. You can pass the
#' output of \code{balassa_index()} here or a similar data structure.
#' @param x column with the elements of set X (applies only if d is a
#' tibble/data.frame)
#' @param y column with the elements of set Y (applies only if d is a
#' tibble/data.frame)
#' @param v column with a metric of the relation between the elements of the
#' sets X and Y (applies only if d is a tibble/data.frame)
#' @param method string to indicate to use one of these methods: reflections,
#' eigenvalues or fitness (default set to "fitness")
#' @param iterations number of iterations (default set to 20, all the methods
#' use this but "eigenvalues" uses it indirectly in sign correction)
#' @param extremality parameter to use in the fitness method
#' (default set to 1, the other methods ignore this parameter)
#' @param tbl when set to TRUE, the output will be a tibble instead of a
#' matrix (default set to TRUE)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate arrange pull rename
#' @importFrom tidyr gather spread
#' @importFrom tibble tibble as_tibble enframe
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym
#'
#' @examples
#' complexity_measures(
#'   bi = binet_output$balassa_index, x = "country", y = "product", v = "value"
#' )
#'
#' @references
#' For more information on this index see:
#'
#' \insertRef{measuringcomplexity2015}{binet}
#'
#' and the references therein.
#'
#' @keywords functions

complexity_measures <- function(bi = NULL,
                                x = NULL,
                                y = NULL,
                                v = "value",
                                method = "fitness",
                                iterations = 20,
                                extremality = 1,
                                tbl = TRUE) {
  # sanity checks ----
  if (all(class(bi) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix",
                               "dgCMatrix") == FALSE)) {
    stop("bi must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (!(any(method %in% c("reflections", "eigenvalues", "fitness")) == TRUE)) {
    stop("method must be reflections, eigenvalues or fitness")
  }

  if (is.integer(iterations) & !iterations >= 2) {
    stop("iterations must be integer and greater or equal to 2")
  }

  if (!is.logical(tbl)) {
    stop("tbl must be logical")
  }

  # convert data.frame input to matrix ----
  if (is.data.frame(bi)) {
    m <- tidyr::spread(bi, !!sym(y), !!sym(v))
    m_rownames <- dplyr::select(m, !!sym(x)) %>% dplyr::pull()

    m <- dplyr::select(m, -!!sym(x)) %>% as.matrix()
    m[is.na(m)] <- 0
    rownames(m) <- m_rownames

    m <- Matrix::Matrix(m, sparse = TRUE)
    m <- m[Matrix::rowSums(m) != 0, Matrix::colSums(m) != 0]
  } else {
    m <- bi[Matrix::rowSums(bi) != 0, Matrix::colSums(bi) != 0]
  }

  # compute complexity measures ----
  # diversity (kx0) and ubiquity (ky0)
  kx0 <- Matrix::rowSums(m)
  ky0 <- Matrix::colSums(m)

  # reflections is defined as a function as these steps are also used for
  # eigenvalues method
  reflections <- function() {
    # create empty matrices
    kx <- Matrix::Matrix(0,
      nrow = length(kx0), ncol = iterations,
      sparse = TRUE
    )

    ky <- Matrix::Matrix(0,
      nrow = length(ky0), ncol = iterations,
      sparse = TRUE
    )

    # fill the first columns with kx0 and ky0 to start iterating
    kx[, 1] <- kx0
    ky[, 1] <- ky0

    # compute cols 2 to "no. of iterations" by iterating from col 1
    for (j in 2:ncol(kx)) {
      kx[, j] <- (m %*% ky[, (j - 1)]) * (1 / kx0)
      ky[, j] <- (Matrix::t(m) %*% kx[, (j - 1)]) * (1 / ky0)
    }

    # xci is of odd order and normalized
    xci <- (kx[, iterations - 1] - base::mean(kx[, iterations - 1])) /
      stats::sd(kx[, iterations - 1])

    # yci is of even order and normalized
    yci <- (ky[, iterations] - base::mean(ky[, iterations])) /
      stats::sd(ky[, iterations])

    names(xci) <- rownames(m)
    names(yci) <- colnames(m)

    return(list(xci = xci, yci = yci))
  }

  if (method == "reflections") {
    reflections_output <- reflections()
    xci <- reflections_output$xci
    yci <- reflections_output$yci
  }

  if (method == "eigenvalues") {
    # to check if a sign correction is needed
    reflections_output <- reflections()
    xci_r <- reflections_output$xci
    yci_r <- reflections_output$yci

    # compute eigenvalues for xci
    xci <- eigen((m %*% (Matrix::t(m) * (1 / ky0))) * (1 / kx0))
    xci <- Re(xci$vectors[, 2])

    # normalized xci
    xci <- (xci - base::mean(xci)) / stats::sd(xci)
    names(xci) <- rownames(m)

    # correct xci sign when required
    if (isTRUE(stats::cor(xci, xci_r, use = "pairwise.complete.obs") < 0)) {
      xci <- (-1) * xci
    }

    # compute eigenvalues for yci
    yci <- eigen((Matrix::t(m) %*% (m * (1 / kx0))) * (1 / ky0))
    yci <- Re(yci$vectors[, 2])

    # normalized yci
    yci <- (yci - base::mean(yci)) / stats::sd(yci)
    names(yci) <- colnames(m)

    # correct yci sign when required
    if (isTRUE(stats::cor(yci, yci_r, use = "pairwise.complete.obs") < 0)) {
      yci <- (-1) * yci
    }
  }

  if (method == "fitness") {
    # create empty matrices
    kx <- Matrix::Matrix(0,
      nrow = length(kx0), ncol = iterations,
      sparse = TRUE
    )

    ky <- Matrix::Matrix(0,
      nrow = length(ky0), ncol = iterations,
      sparse = TRUE
    )

    # fill the first columns with kx0 and ky0 to start iterating
    kx[, 1] <- 1
    ky[, 1] <- 1

    # compute cols 2 to "no. of iterations" by iterating from col 1
    for (j in 2:ncol(kx)) {
      kx[, j] <- m %*% ky[, (j - 1)]

      kx[, j] <- kx[, j] / mean(kx[, j])

      ky[, j] <- 1 / (Matrix::t(m) %*%
        (1 / kx[, (j - 1)])^extremality)^(1 / extremality)

      ky[, j] <- ky[, j] / mean(ky[, j])
    }

    xci <- kx[, iterations]

    yci <- ky[, iterations]

    names(xci) <- rownames(m)

    names(yci) <- colnames(m)
  }

  if (tbl == TRUE) {
    xci <- tibble::tibble(v = xci) %>%
      dplyr::mutate(x = names(xci)) %>%
      dplyr::select(!!sym("x"), !!sym("v")) %>%
      dplyr::arrange(-!!sym("v")) %>%
      dplyr::rename(
        "country" = !!sym("x"),
        "value" = !!sym("v")
      )

    yci <- tibble::tibble(v = yci) %>%
      dplyr::mutate(y = names(yci)) %>%
      dplyr::select(!!sym("y"), !!sym("v")) %>%
      dplyr::arrange(-!!sym("v")) %>%
      dplyr::rename(
        "product" = !!sym("y"),
        "value" = !!sym("v")
      )

    kx0 <- tibble::enframe(kx0) %>%
      dplyr::rename("country" = !!sym("name"))

    ky0 <- tibble::enframe(ky0) %>%
      dplyr::rename("product" = !!sym("name"))
  }

  return(
    list(
      complexity_index_x = xci,
      complexity_index_y = yci,
      diversity = kx0,
      ubiquity = ky0
    )
  )
}
