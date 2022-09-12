#' @rdname mixingmatrix
#' @title Extract Mixing Matrix from \code{networkLite}
#' @param object A \code{networkLite} object.
#' @param attr The name of a vertex attribute in \code{object}.
#' @param ... additional arguments
#' @return The mixing matrix for \code{object} and \code{attr}.
#' @export
mixingmatrix.networkLite <- function(object, attr, ...) {
  nw <- object

  all_attr <- get.vertex.attribute(nw, attr)

  if (is.bipartite(nw)) {
    row_levels <- sort(unique(all_attr[seq_len(nw %n% "bipartite")]))
    col_levels <- sort(unique(all_attr[-seq_len(nw %n% "bipartite")]))
  } else {
    row_levels <- sort(unique(all_attr))
    col_levels <- row_levels
  }

  el <- as.edgelist(nw)

  m <- table(from = factor(all_attr[el[, 1]], levels = row_levels),
             to = factor(all_attr[el[, 2]], levels = col_levels))

  if (!is.bipartite(nw) && !is.directed(nw)) {
    m <- m + t(m) - diag(diag(m))
  }

  m
}
