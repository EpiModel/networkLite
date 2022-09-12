#' @rdname as_networkLite
#' @title Convert to \code{networkLite} Representation
#' @details \code{as.networkLite.network} converts a \code{network} object
#'          to a \code{networkLite} object.  \code{as.networkLite.networkLite}
#'          returns the \code{networkLite} object unchanged.
#'
#'          Currently the network attributes \code{hyper}, \code{multiple}, and
#'          \code{loops} must be \code{FALSE} for \code{networkLite}s;
#'          attempting to convert a \code{network} to a \code{networkLite} when
#'          this is not the case will result in an error.
#' @param x A \code{network} or \code{networkLite} object.
#' @param ... additional arguments
#' @return A corresponding \code{networkLite} object.
#' @seealso \code{\link{to_network_networkLite}}
#' @export
as.networkLite <- function(x, ...) {
  UseMethod("as.networkLite")
}

#' @rdname as_networkLite
#' @export
as.networkLite.network <- function(x, ...) {
  if (is.hyper(x) || is.multiplex(x) || has.loops(x)) {
    stop("cannot coerce `network` to `networkLite` unless `hyper`,",
         " `multiple`, and `loops` are all `FALSE`")
  }

  el <- as.edgelist(x, na.rm = FALSE)

  rv <- networkLite(el)

  for (name in list.vertex.attributes(x)) {
    set.vertex.attribute(rv, name, get.vertex.attribute(x,
                                                        name,
                                                        null.na = TRUE,
                                                        unlist = FALSE))
  }

  for (name in setdiff(list.network.attributes(x), c("mnext"))) {
    set.network.attribute(rv, name, get.network.attribute(x, name))
  }

  eids <- unlist(get.dyads.eids(x, el[, 1], el[, 2]))
  for (name in list.edge.attributes(x)) {
    set.edge.attribute(rv, name, get.edge.attribute(x, name, null.na = TRUE,
                                                    deleted.edges.omit = FALSE,
                                                    unlist = FALSE)[eids])
  }

  for (name in setdiff(names(attributes(x)), c("class", "names"))) {
    attr(rv, name) <- attr(x, name)
  }

  rv <- atomize(rv)

  rv
}

## convert vertex and edge attributes to atomic vectors where possible;
## note that this may upcast atomic types, e.g. logical -> numeric -> character
atomize <- function(nwL) {
  for (name in list.vertex.attributes(nwL)) {
    value <- get.vertex.attribute(nwL, name, unlist = FALSE)
    if (length(value) > 0 &&
        all(unlist(lapply(value, is.atomic))) &&
        all(unlist(lapply(value, length)) == 1)) {
      nwL$attr[[name]] <- unlist(value)
    }
  }

  for (name in list.edge.attributes(nwL)) {
    value <- get.edge.attribute(nwL, name, unlist = FALSE)
    if (length(value) > 0 &&
        all(unlist(lapply(value, is.atomic))) &&
        all(unlist(lapply(value, length)) == 1)) {
      nwL$el[[name]] <- unlist(value)
    }
  }

  nwL
}

#' @rdname as_networkLite
#' @export
as.networkLite.networkLite <- function(x, ...) {
  x
}
