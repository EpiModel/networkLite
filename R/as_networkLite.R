#' @rdname as_networkLite
#'
#' @title Convert to \code{networkLite} Representation.
#'
#' @param x A \code{network} or \code{networkLite} object.
#' @param atomize Logical; should we call \code{\link{atomize}} on the
#'        \code{networkLite} before returning it?
#' @param ... additional arguments
#'
#' @details \code{as.networkLite.network} converts a \code{network} object
#' to a \code{networkLite} object. \code{as.networkLite.networkLite}
#' returns the \code{networkLite} object unchanged.
#'
#' Currently the network attributes \code{hyper}, \code{multiple}, and
#' \code{loops} must be \code{FALSE} for \code{networkLite}s;
#' attempting to convert a \code{network} to a \code{networkLite} when
#' this is not the case will result in an error.
#'
#' The \code{...} are passed to \code{\link{atomize}} and can be used
#' to set the \code{upcast} argument controlling attribute conversion.
#'
#' @return A corresponding \code{networkLite} object.
#'
#' @seealso \code{\link{to_network_networkLite}}
#'
#' @export
#'
as.networkLite <- function(x, ...) {
  UseMethod("as.networkLite")
}

#' @rdname as_networkLite
#' @export
as.networkLite.network <- function(x, ..., atomize = TRUE) {
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

  eids <- unlist(lapply(seq_len(NROW(el)),
                 function(index) {
                   get.edgeIDs(x, el[index, 1], el[index, 2], na.omit = FALSE)
                 }))
  for (name in list.edge.attributes(x)) {
    value <- get.edge.attribute(x, name, unlist = FALSE, null.na = FALSE,
                                na.omit = FALSE, deleted.edges.omit = FALSE)[eids]
    set.edge.attribute(rv, name, value)
  }

  for (name in setdiff(names(attributes(x)), c("class", "names"))) {
    attr(rv, name) <- attr(x, name)
  }

  if (atomize == TRUE) {
    rv <- atomize(rv, ...)
  }

  rv
}

#' @rdname as_networkLite
#' @export
as.networkLite.networkLite <- function(x, ...) {
  x
}
