
#' @rdname constructors
#' @title networkLite Constructor Utilities
#'
#' @description Constructor methods for \code{networkLite} objects.
#'
#' @param x Either an \code{edgelist} class network representation, including
#'        network attributes as \code{attr}-style attributes on the
#'        \code{edgelist}, or a number specifying the network size. The
#'        \code{edgelist} may be either a \code{tibble} or a \code{matrix}. If
#'        a \code{tibble} is passed, it should have integer columns named
#'        \code{".tail"} and \code{".head"} for the tails and heads of edges,
#'        and may include edge attributes as additional columns. If a
#'        \code{matrix} is passed, it should have two columns, the first being
#'        the tails of edges and the second being the heads of edges; edge
#'        attributes are not supported for \code{matrix} arguments. Edges
#'        should be sorted, first on tails then on heads. See
#'        \code{\link[network]{as.edgelist}} for information on producing such
#'        \code{edgelist} objects from \code{network} objects.
#'
#'        The \code{edgelist} \emph{must} have the \code{"n"} attribute
#'        indicating the network size, and may include additional named
#'        \code{attr}-style attributes that will be interpreted as network
#'        attributes and copied to the \code{networkLite}. Exceptions to this
#'        are attributes named \code{"class"}, \code{"dim"}, \code{"dimnames"},
#'        \code{"vnames"}, \code{"row.names"}, \code{"names"}, and
#'        \code{"mnext"}; these are not copied from the \code{edgelist} to the
#'        \code{networkLite}.
#' @param attr A named list of vertex attributes, coerced to \code{tibble}.
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
#' \code{gal}. The fields \code{el} and \code{attr} are \code{tibble}s
#' corresponding to the \code{x} and \code{attr} arguments, respectively, and
#' the field \code{gal} is the list of network attributes (copied from
#' \code{attributes(x)}, with the exceptions noted above). Missing network
#' attributes \code{directed} and \code{bipartite} are defaulted to
#' \code{FALSE}; the network size attribute \code{n} must not be missing.
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
#' Within \code{EpiModel}, the \code{networkLite} data structure is used in the
#' calls to \code{ergm} and \code{tergm} \code{simulate} and \code{summary}
#' functions.
#'
#' @return
#' A \code{networkLite} object with edgelist \code{el}, vertex attributes
#' \code{attr}, and network attributes \code{gal}.
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

  if (is_tibble(x)) {
    el <- x
  } else {
    el <- as_tibble(list(.tail = as.integer(x[, 1]),
                         .head = as.integer(x[, 2])))
  }

  nw <- list(el = el,
             attr = as_tibble(attr),
             gal = attributes(x)[setdiff(names(attributes(x)),
                                         c("class", "dim", "dimnames",
                                           "vnames", "row.names", "names",
                                           "mnext"))])

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
