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
    value <- get.vertex.attribute(x, name, null.na = FALSE, na.omit = FALSE,
                                  unlist = FALSE)
    set.vertex.attribute(rv, name, value)
  }

  for (name in setdiff(list.network.attributes(x), c("mnext"))) {
    value <- get.network.attribute(x, name)
    set.network.attribute(rv, name, value)
  }

  eids <- unlist(lapply(seq_len(NROW(el)), function(index) get.edgeIDs(x, el[index, 1], el[index, 2], na.omit = FALSE)))
  for (name in list.edge.attributes(x)) {
    value <- get.edge.attribute(x, name, unlist = FALSE, null.na = FALSE,
                                na.omit = FALSE, deleted.edges.omit = FALSE)[eids]
    set.edge.attribute(rv, name, value)
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
  nwL$el <- atomize_tibble(nwL$el) # also applies to .tail, .head
  nwL$attr <- atomize_tibble(nwL$attr)
  nwL
}

atomize_tibble <- function(x) {
  for (name in names(x)) {
    value <- x[[name]]
    if (length(value) > 0 &&
        all(unlist(lapply(value, is.atomic))) &&
        all(unlist(lapply(value, length)) == 1)) {
      x[[name]] <- unlist(value)
    }
  }
  x
}

#' @rdname as_networkLite
#' @export
as.networkLite.networkLite <- function(x, ...) {
  x
}
