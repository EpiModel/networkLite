
#' @rdname to_network_networkLite
#'
#' @title Convert a `networkLite` object to a `network` object
#'
#' @param x A `networkLite` object.
#' @param ... additional arguments.
#'
#' @details
#' The `to_network_networkLite` function takes a `networkLite` and returns a
#' corresponding `network`.
#'
#' The `as.network.networkLite` method returns the `networkLite` unchanged, for
#' compatibility with `ergm`.
#'
#' @return
#' For `to_network_networkLite`, a `network` object corresponding to `x` is returned. For
#' `as.network.networkLite`, the `networkLite` `x` is returned unchanged.
#'
#' @seealso [`as.networkLite`]
#'
#' @export
#'
to_network_networkLite <- function(x, ...) {
  nw <- network.initialize(network.size(x),
                           directed = x %n% "directed",
                           bipartite = x %n% "bipartite")

  el <- as.edgelist(x, na.rm = FALSE)

  add.edges(nw, el[, 1], el[, 2])

  for (name in list.vertex.attributes(x)) {
    value <- get.vertex.attribute(x, name, null.na = FALSE, unlist = FALSE)
    set.vertex.attribute(nw, name, value)
  }

  for (name in list.network.attributes(x)) {
    value <- get.network.attribute(x, name)
    set.network.attribute(nw, name, value)
  }

  eids <- unlist(lapply(seq_len(NROW(el)),
                        function(index) get.edgeIDs(nw, el[index, 1], el[index, 2], na.omit = FALSE)))
  for (name in list.edge.attributes(x)) {
    value <- get.edge.attribute(x, name, null.na = FALSE, unlist = FALSE)
    set.edge.attribute(nw, name, value, eids)
  }

  for (name in setdiff(names(attributes(x)), c("class", "names"))) {
    attr(nw, name) <- attr(x, name)
  }

  # "Pop" the first networkLite class definition.
  structure(nw, class = class(x)[-which(class(x)=="networkLite")[1]])
}

#' @rdname to_network_networkLite
#' @export
as.network.networkLite <- function(x, ...) {
  return(x)
}

#' @rdname last-resort
#'
#' @title Last-resort utilities
#'
#' @description Since the purpose of [`networkLite`] is to be a drop-in replacement
#' for [`network`], these utilities emulate the internal structure of
#' `network`. They are likely to perform poorly and should be avoided.
#'
#' @param x a `networkLite` object
#' @param name passed to `[[`, but some special cases (`"oel"` nd `"iel"` are handled)
#'
#' @note Please do not rely on these unless you absolutely have to;
#'   since they slow down access to `networkLite` objects, they may be
#'   removed if they ever stop being necessary.
#'
#' @export
`$.networkLite` <- function(x, name) {
  o <- x[[name, exact = FALSE]]
  if(!is.null(o)) return(o)

  name <- match.arg(name, c("oel", "iel"))
  if(getOption("networkLite.warn_fallback")) warning(sQuote("networkLite"), " fallback invoked: ", sQuote(name), ".", immediate. = TRUE)
  switch(name,
         oel = split(seq_len(nrow(x$el)), factor(x$el$.tail, levels = seq_len(x$gal$n))),
         iel = split(seq_len(nrow(x$el)), factor(x$el$.head, levels = seq_len(x$gal$n)))
         )
}

#' @rdname last-resort
#' @export
`[.networkLite` <- function(x, ...) {
  if(getOption("networkLite.warn_fallback")) warning(sQuote("networkLite"), " fallback invoked: ", sQuote("["), ".", immediate. = TRUE)
  to_network_networkLite(x)[...]
}
