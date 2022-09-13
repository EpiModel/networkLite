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

      new_names <- unique(unlist(lapply(vattr, names)))
      for (i in seq_along(vattr)) {
        given_names <- names(vattr[[i]])
        null_names <- setdiff(new_names, given_names)
        vattr[[i]] <- c(vattr[[i]], vector(mode = "list", length = length(null_names)))
        names(vattr[[i]]) <- c(given_names, null_names)
      }
      
      update_list <- lapply(new_names, function(name) lapply(vattr, `[[`, name))
      names(update_list) <- new_names
      update_tibble <- as_tibble(update_list)

      if ("na" %in% new_names) {
        update_tibble[["na"]] <- lapply(update_tibble[["na"]],
                                        function(val) if (is.null(val) || is.na(val)) FALSE else val)
      } else {
        new_names <- c(new_names, "na")
        update_tibble[["na"]] <- logical(NROW(update_tibble))
      }
    } else {
      new_names <- c("na")
      update_tibble <- as_tibble(list(na = logical(nv)))
    }

    old_names <- list.vertex.attributes(x)
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
