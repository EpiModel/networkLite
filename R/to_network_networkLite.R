#' @rdname to_network_networkLite
#' @title Convert a \code{networkLite} object to a \code{network} object
#' @param x A \code{networkLite} object.
#' @param ... additional arguments
#' @return A corresponding \code{network} object.
#' @seealso \code{\link{as.networkLite}}
#' @details The \code{to_network_networkLite} function takes a
#'          \code{networkLite} and returns a corresponding \code{network}.
#'
#'          The \code{as.network.networkLite} method returns the
#'          \code{networkLite} unchanged, for compatibility with \code{ergm}.
#' @export
to_network_networkLite <- function(x, ...) {
  nw <- network.initialize(network.size(x),
                           directed = x %n% "directed",
                           bipartite = x %n% "bipartite")

  el <- as.edgelist(x, na.rm = FALSE)

  nw <- add.edges(nw, el[, 1], el[, 2])

  for (name in list.vertex.attributes(x)) {
    set.vertex.attribute(nw, name, get.vertex.attribute(x, name, unlist = FALSE))
  }

  for (name in list.network.attributes(x)) {
    set.network.attribute(nw, name, get.network.attribute(x, name))
  }

  eids <- unlist(get.dyads.eids(nw, el[, 1], el[, 2]))
  for (name in list.edge.attributes(x)) {
    set.edge.attribute(nw, name, get.edge.attribute(x, name, unlist = FALSE), eids)
  }

  for (name in setdiff(names(attributes(x)), c("class", "names"))) {
    attr(nw, name) <- attr(x, name)
  }

  nw
}

#' @rdname to_network_networkLite
#' @export
as.network.networkLite <- function(x, ...) {
  return(x)
}
