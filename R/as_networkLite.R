
#' @rdname as_networkLite
#'
#' @title Convert to `networkLite` Representation.
#'
#' @param x A `network` or `networkLite` object.
#' @param atomize Logical; should we call [`atomize`] on the
#'        `networkLite` before returning it?
#' @param ... additional arguments
#'
#' @details `as.networkLite.network` converts a `network` object
#' to a `networkLite` object. `as.networkLite.networkLite`
#' returns the `networkLite` object unchanged.
#'
#' Currently the network attributes `hyper`, `multiple`, and
#' `loops` must be `FALSE` for `networkLite`s;
#' attempting to convert a `network` to a `networkLite` when
#' this is not the case will result in an error.
#'
#' The `...` are passed to [`atomize`] and can be used
#' to set the `upcast` argument controlling attribute conversion.
#'
#' @return
#' A corresponding `networkLite` object.
#'
#' @seealso [`to_network_networkLite`]
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
