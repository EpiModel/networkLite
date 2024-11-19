
#' @rdname add_vertices
#'
#' @title Add Vertices to a `networkLite`.
#'
#' @param x A `networkLite` object.
#' @param nv Number of vertices to add to the `networkLite`.
#' @param vattr A list (of length `nv`) of named lists of vertex
#'        attribute values for added vertices, or `NULL` to indicate vertex
#'        attribute values are not being passed.
#' @param last.mode logical; if `x` is bipartite, should the new vertices
#'        be added to the second mode?
#' @param ... additional arguments
#'
#' @return A `networkLite` object with vertices added.
#'
#' @export
#'
add.vertices.networkLite <- function(x, nv, vattr = NULL,
                                     last.mode = TRUE, ...) {
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

  modify_in_place(x)
}

#' Permute vertices
#'
#' @param x,vids,... see [network::permute.vertexIDs()]
#' @export
permute.vertexIDs.networkLite <- function(x, vids, ...) {
  #Sanity check: is this a permutation vector?
  n<-network.size(x)
  if((length(unique(vids))!=n)||any(range(vids)!=c(1,n)))
    stop("Invalid permutation vector in permute.vertexIDs.")
  if(is.bipartite(x)){  #If bipartite, enforce partitioning
    bpc<-get.network.attribute(x,"bipartite")
    if(any(vids[0:bpc]>bpc)||any(vids[(bpc+1):n]<=bpc))
      warning("Performing a cross-mode permutation in permute.vertexIDs.  I hope you know what you're doing....")
  }

  # Remap the edge list and sort by new indices.
  o <- order(vids)
  x$el$.tail <- o[x$el$.tail]
  x$el$.head <- o[x$el$.head]
  if(!is.directed(x)) x$el[, c(".tail", ".head")] <- cbind(pmin(x$el$.tail, x$el$.head), pmax(x$el$.tail, x$el$.head))
  x$el <- x$el[order(x$el$.tail, x$el$.head), , drop = FALSE]

  # Permute the vertex attributes.
  x$attr <- x$attr[vids, , drop = FALSE]

  modify_in_place(x)
}
