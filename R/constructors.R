
#' @rdname constructors
#' @title networkLite Constructor Utilities
#'
#' @description Constructor methods for \code{networkLite} objects.
#'
#' @param x Either an \code{edgelist} class network representation (including
#'        network attributes in its \code{attributes} list), or a number
#'        specifying the network size.
#' @param attr A named list of vertex attributes for the network represented by
#'        \code{x}.
#' @param directed,bipartite Common network attributes that may be set via
#'        arguments to the \code{networkLite.numeric} method.
#' @param ... additional arguments
#'
#' @details Currently there are several distinct \code{networkLite} constructor
#' methods available.
#'
#' The \code{edgelist} method takes an \code{edgelist} class object \code{x}
#' with network attributes attached in its \code{attributes} list, and a named
#' list of vertex attributes \code{attr}, and returns a \code{networkLite}
#' object, which is a named list with fields \code{el}, \code{attr}, and
#' \code{gal}; the fields \code{el} and \code{attr} match the arguments \code{x}
#' and \code{attr} (the latter coerced to \code{tibble}) respectively, and the
#' field \code{gal} is the list of network attributes (copied from
#' \code{attributes(x)}). Missing network attributes \code{directed} and
#' \code{bipartite} are defaulted to \code{FALSE}; the network size attribute
#' \code{n} must not be missing. Attributes \code{class}, \code{dim},
#' \code{dimnames}, \code{vnames}, and \code{mnext} (if present) are not copied
#' from \code{x} to the \code{networkLite}.  (For convenience, a \code{matrix}
#' method, identical to the \code{edgelist} method, is also defined, to handle
#' cases where the edgelist is, for whatever reason, not classed as an
#' \code{edgelist}.)
#'
#' The \code{numeric} method takes a number \code{x} as well as the network
#' attributes \code{directed} and \code{bipartite} (defaulting to \code{FALSE}),
#' and returns an empty \code{networkLite} with these network attributes and
#' number of nodes \code{x}.
#'
#' The constructor \code{networkLite_initialize} is also available for creating
#' an empty \code{networkLite}, and its \code{x} argument should be a number
#' indicating the size of the \code{networkLite} to create.
#'
#' Within \code{tergmLite}, the \code{networkLite} data structure is used in the
#' calls to \code{ergm} and \code{tergm} \code{simulate} functions.
#'
#' @return
#' A networkLite object with edge list \code{el}, vertex attributes \code{attr},
#' and network attributes \code{gal}.
#'
#' @export
#'
#' @examples
#' edgelist <- cbind(c(1,2,3), c(2,4,7))
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
    attr = list(vertex.names = seq_len(attributes(x)[["n"]]),
    na = logical(attributes(x)[["n"]])),
    ...) {

  nw <- list(el = x,
             attr = as_tibble(attr),
             gal = attributes(x)[setdiff(names(attributes(x)),
                                         c("class", "dim", "dimnames",
                                           "vnames", "mnext"))])

  if (!is_tibble(x)) {
    nw$el <- as_tibble(list(.tail = as.integer(x[, 1]),
                            .head = as.integer(x[, 2])))
  }

  nw$el[["na"]] <- NVL(nw$el[["na"]], logical(NROW(nw$el)))
  nw$el[["na"]][is.na(nw$el[["na"]])] <- FALSE

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
