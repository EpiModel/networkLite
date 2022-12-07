
#' @rdname delete_edges
#' @title Delete edges from a networkLite.
#' @param x A \code{networkLite} object.
#' @param eid Edge ids (between \code{1} and
#'            \code{network.edgecount(x, na.omit = FALSE)}) to delete in
#'            \code{x}. Note that the edge id of an edge in \code{x} is simply
#'            its row index in \code{x$el}.
#' @param ... additional arguments.
#' @export
delete.edges.networkLite <- function(x, eid, ...) {
  xn <- substitute(x)

  eid <- as.integer(eid)
  eid <- eid[eid >= 1 & eid <= network.edgecount(x, na.omit = FALSE)]
  if (length(eid) > 0) {
    x$el <- x$el[-eid, ]
  }

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}
