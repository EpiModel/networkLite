#' @rdname delete_vertices
#' @title Delete vertices from a networkLite.
#' @param x A \code{networkLite} object.
#' @param vid Vertex ids (between \code{1} and \code{network.size(x)}) to delete
#'            from \code{x}. Note that edges involving deleted vertices will
#'            also be deleted.
#' @export
delete.vertices.networkLite <- function(x, vid, ...) {
  xn <- substitute(x)

  vid <- as.integer(vid)
  vid <- vid[vid >= 1 & vid <= network.size(x)]
  if (length(vid) > 0) {
    # drop edges with deleted nodes
    x$el <- x$el[!(x$el$.tail %in% vid | x$el$.head %in% vid), ]

    # drop vertex attributes for deleted nodes
    x$attr <- x$attr[-vid, ]

    # remap nodal indices for remaining edges
    a <- seq_len(network.size(x))
    b <- integer(network.size(x))
    b[vid] <- 1L
    b <- cumsum(b)
    a <- a - b
    x$el$.tail <- a[x$el$.tail]
    x$el$.head <- a[x$el$.head]

    # update network attributes
    x %n% "n" <- x %n% "n" - length(vid)
    if (is.bipartite(x)) {
      x %n% "bipartite" <- x %n% "bipartite" - sum(vid <= x %n% "bipartite")
    }
  }

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}
