
#' @rdname delete_edges
#'
#' @title Delete edges from a networkLite.
#'
#' @param x A `networkLite` object.
#' @param eid Edge ids (between `1` and
#'            `network.edgecount(x, na.omit = FALSE)`) to delete in
#'            `x`. Note that the edge id of an edge in `x` is simply
#'            its row index in `x$el`.
#' @param ... additional arguments.
#'
#' @return A `networkLite` object with the specified edges deleted.
#'
#' @export
#'
delete.edges.networkLite <- function(x, eid, ...) {
  eid <- as.integer(eid)
  eid <- eid[eid >= 1 & eid <= network.edgecount(x, na.omit = FALSE)]
  if (length(eid) > 0) {
    x$el <- x$el[-eid, ]
  }

  modify_in_place(x)
}
