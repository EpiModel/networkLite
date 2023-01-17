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

    ## if we were passed any attribute information...
    if (length(unlist(vattr)) > 0) {
      if (is.list(vattr)) {
        vattr <- rep(vattr, length.out = nv)
      } else {
        vattr <- as.list(rep(vattr, length.out = nv))
      }

      new_names <- unique(unlist(lapply(vattr, names)))
      update_list <- lapply(new_names, function(name) lapply(vattr, `[[`, name))
      names(update_list) <- new_names
    } else {
      update_list <- list()
    }

    if ("na" %in% names(update_list)) {
      update_list[["na"]] <- lapply(update_list[["na"]], isTRUE)
    } else {
      update_list <- c(update_list, list(na = logical(nv)))
    }
    update_tibble <- as_tibble(update_list)

    new_names <- names(update_tibble) # including "na"
    old_names <- names(x$attr)

    for (name in setdiff(old_names, new_names)) {
      update_tibble[[name]] <- vector(mode = "list", length = NROW(update_tibble))
    }
    for (name in setdiff(new_names, old_names)) {
      x$attr[[name]] <- vector(mode = "list", length = NROW(x$attr))
    }

    x$attr <- dplyr::bind_rows(ensure_list(list(x$attr[seq_len(offset), ],
                                                update_tibble,
                                                x$attr[offset + seq_len(oldsize - offset), ])))
  }

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}
