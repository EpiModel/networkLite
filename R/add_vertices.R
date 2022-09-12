#' @rdname add_vertices
#' @title Add Vertices to a \code{networkLite}
#' @param x A \code{networkLite} object.
#' @param nv Number of vertices to add to the \code{networkLite}.
#' @param vattr A list (of length \code{nv}) of named lists of vertex
#'        attribute values for added vertices, or \code{NULL} to indicate vertex
#'        attribute values are not being passed.
#' @param last.mode logical; if \code{x} is bipartite, should the new vertices
#'        be added to the second mode?
#' @param ... additional arguments
#' @export
add.vertices.networkLite <- function(x, nv, vattr = NULL,
                                     last.mode = TRUE, ...) {
  xn <- substitute(x)

  nv <- as.integer(nv)
  if (nv > 0) {
    oldsize <- network.size(x)
    x %n% "n" <- oldsize + nv

    if (is.bipartite(x) && !last.mode) {
      offset <- x %n% "bipartite"
      x %n% "bipartite" <- x %n% "bipartite" + nv
      x$el$.head <- x$el$.head + nv
    } else {
      offset <- oldsize
    }

    if (!is.null(vattr)) {
      if (is.list(vattr)) {
        vattr <- rep(vattr, length.out = nv)
      } else {
        vattr <- as.list(rep(vattr, length.out = nv))
      }

      f <- function(x) if (length(x) > 0) as_tibble(x) else tibble(NULL, .rows = 1)
      update_tibble <- dplyr::bind_rows(lapply(vattr, f))
    } else {
      update_tibble <- as_tibble(list(na = logical(nv)))
    }
    update_tibble[["na"]] <- NVL(update_tibble[["na"]],
                                 logical(NROW(update_tibble)))
    update_tibble[["na"]][is.na(update_tibble[["na"]])] <- FALSE

    x$attr <- dplyr::bind_rows(x$attr[seq_len(offset), ],
                               update_tibble,
                               x$attr[offset + seq_len(oldsize - offset), ])
  }

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}
