
#' @rdname constructors
#'
#' @title networkLite Constructor Utilities
#'
#' @description Constructor methods for `networkLite` objects.
#'
#' @param x Either an `edgelist` class network representation, or a number
#'        specifying the network size. The `edgelist` may be either a `tibble`
#'        or a `matrix`. If a `tibble` is passed, it should have integer columns
#'        named `".tail"` and `".head"` for the tails and heads of edges,
#'        and may include edge attributes as additional columns. If a
#'        `matrix` is passed, it should have two columns, the first being
#'        the tails of edges and the second being the heads of edges; edge
#'        attributes are not supported for `matrix` arguments. Edges
#'        should be sorted, first on tails then on heads. See
#'        [`network::as.edgelist`] for information on producing such
#'        `edgelist` objects from `network` objects.
#' @param attr A named list of vertex attributes, coerced to `tibble`.
#'        Each element of `attr` should be an atomic vector or list of
#'        length equal to the number of nodes in the network.
#' @param net_attr A named list of network attributes. Must include the network
#'        size attribute named `"n"`. Defaults to a subset of the `attr`-style
#'        attributes of `x` for backwards compatibility; it is recommended that
#'        new code specify `net_attr` explicitly rather than relying on this
#'        default.
#' @param directed,bipartite Common network attributes that may be set via
#'        arguments to the `networkLite.numeric` method.
#' @param atomize Logical; should we call [`atomize`] on the
#'        `networkLite` before returning it? Note that unlike
#'        [`as.networkLite`], the default value here is `FALSE`.
#' @param ... additional arguments
#' @return A `networkLite` object constructed according to the inputs.
#'
#' @details Currently there are several distinct `networkLite` constructor
#' methods available.
#'
#' The `edgelist` method takes an `edgelist` class object `x`, a named
#' list of vertex attributes `attr`, and a named list of network attributes
#' `net_attr`, and returns a `networkLite` object, which is a named list with
#' fields `el`, `attr`, and `gal`, corresponding to the arguments `x`, `attr`,
#' and `net_attr`. Missing network attributes `directed` and `bipartite` are
#' defaulted to `FALSE`; the network size attribute `n` must not be missing.
#'
#' The `numeric` method takes a number `x` as well as the network
#' attributes `directed` and `bipartite` (defaulting to `FALSE`),
#' and returns an empty `networkLite` with these network attributes and
#' number of nodes `x`.
#'
#' The constructor `networkLite_initialize` is also available for creating
#' an empty `networkLite`, and its `x` argument should be a number
#' indicating the size of the `networkLite` to create.
#'
#' Within `EpiModel`, the `networkLite` data structure is used in the
#' calls to `ergm` and `tergm` `simulate` and `summary`
#' functions.
#'
#' @export
#'
#' @examples
#' edgelist <- cbind(c(1, 2, 3), c(2, 4, 7))
#' attr(edgelist, "n") <- 10 # network size
#' vertex_attributes <- list(a = 1:10, b = runif(10))
#' nwL <- networkLite(edgelist, vertex_attributes)
#' nwL
#'
networkLite <- function(x, ...) {
  UseMethod("networkLite")
}

#' @rdname constructors
#' @export
networkLite.edgelist <- function(
    x,
    attr = list(vertex.names = seq_len(net_attr[["n"]]),
                na = logical(net_attr[["n"]])),
    net_attr = attributes(x)[setdiff(names(attributes(x)),
                                         c("class", "dim", "dimnames",
                                           "vnames", "row.names", "names",
                                           "mnext"))],
    ...,
    atomize = FALSE) {

  if (is_tibble(x)) {
    if (!(".tail" %in% names(x)) || !(".head" %in% names(x))) {
      stop("tibble edgelist must include column names '.tail' and '.head'")
    }
    el <- x
  } else {
    if (NCOL(x) != 2) {
      stop("matrix edgelist must have two columns")
    }
    el <- as_tibble(list(.tail = as.integer(x[, 1]),
                         .head = as.integer(x[, 2])))
  }

  nw <- list(el = el,
             attr = as_tibble(attr),
             gal = net_attr)

  if ("na" %in% names(nw$el)) {
    if (is.logical(nw$el[["na"]])) {
      nw$el[["na"]][is.na(nw$el[["na"]])] <- FALSE
    } else {
      nw$el[["na"]] <- lapply(nw$el[["na"]], isTRUE)
    }
  } else {
    nw$el[["na"]] <- logical(NROW(nw$el))
  }

  # network size attribute is required
  if (is.null(nw$gal[["n"]])) {
    stop("edgelist passed to networkLite must have the `n` attribute.")
  }
  # other common attributes default to FALSE
  if (is.null(nw$gal[["directed"]])) {
    nw$gal[["directed"]] <- FALSE
  }
  if (is.null(nw$gal[["bipartite"]])) {
    nw$gal[["bipartite"]] <- FALSE
  }
  if (is.null(nw$gal[["loops"]])) {
    nw$gal[["loops"]] <- FALSE
  }
  if (is.null(nw$gal[["hyper"]])) {
    nw$gal[["hyper"]] <- FALSE
  }
  if (is.null(nw$gal[["multiple"]])) {
    nw$gal[["multiple"]] <- FALSE
  }

  if (!isFALSE(nw$gal[["loops"]]) || !isFALSE(nw$gal[["hyper"]]) ||
      !isFALSE(nw$gal[["multiple"]])) {
    stop("networkLite requires network attributes `loops`, `hyper`, and",
         " `multiple` be `FALSE`.")
  }

  ## for consistency with network,
  ## we want nw$gal[["n"]] to be of
  ## type numeric, not integer
  nw$gal[["n"]] <- as.numeric(nw$gal[["n"]])

  class(nw) <- c("networkLite", "network")

  if (atomize == TRUE) {
    nw <- atomize(nw, ...)
  }

  return(nw)
}

#' @rdname constructors
#' @export
networkLite.matrix <- networkLite.edgelist

#' @rdname constructors
#' @export
networkLite.numeric <- function(x,
                                directed = FALSE,
                                bipartite = FALSE,
                                ...) {
  x <- as.numeric(x) # so it's not of class integer

  el <- as_tibble(list(.tail = integer(0), .head = integer(0), na = logical(0)))
  attr <- list(vertex.names = seq_len(x), na = logical(x))
  gal <- list(n = x, directed = directed, bipartite = bipartite,
              loops = FALSE, hyper = FALSE, multiple = FALSE)

  nw <- list(el = el, attr = as_tibble(attr), gal = gal)

  class(nw) <- c("networkLite", "network")
  return(nw)
}

#' @rdname constructors
#' @export
networkLite_initialize <- networkLite.numeric
