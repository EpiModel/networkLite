
# Re-export network functions as S3 generics for networkLite

#' @name get.edgeIDs
#' @export
get.edgeIDs <- function(x, ...) {
  UseMethod("get.edgeIDs")
}

#' @export
get.edgeIDs.default <- function(x, v, alter = NULL, 
                                neighborhood = c("out", "in", "combined"),
                                na.omit = TRUE, ...) {
  network::get.edgeIDs(x, v, alter, neighborhood, na.omit, ...)
}

#' @name get.dyads.eids
#' @export
get.dyads.eids <- function(x, ...) {
  UseMethod("get.dyads.eids")
}

#' @export
get.dyads.eids.default <- function(x, tails, heads,
                                   neighborhood = c("out", "in", "combined"),
                                   na.omit = TRUE, ...) {
  network::get.dyads.eids(x, tails, heads, neighborhood, na.omit, ...)
}

#' @name get.edges
#' @export
get.edges <- function(x, ...) {
  UseMethod("get.edges")
}

#' @export
get.edges.default <- function(x, v, alter, neighborhood = c("combined", "out", "in"),
                              na.omit = TRUE, ...) {
  network::get.edges(x, v, alter, neighborhood, na.omit, ...)
}

#' @name get.neighborhood
#' @export
get.neighborhood <- function(x, ...) {
  UseMethod("get.neighborhood")
}

#' @export
get.neighborhood.default <- function(x, v, type = c("combined", "out", "in"),
                                     na.omit = TRUE, ...) {
  network::get.neighborhood(x, v, type, na.omit, ...)
}

#' @name is.adjacent
#' @export
is.adjacent <- function(x, ...) {
  UseMethod("is.adjacent")
}

#' @export
is.adjacent.default <- function(x, vi, vj, na.omit = FALSE, ...) {
  network::is.adjacent(x, vi, vj, na.omit, ...)
}

#' @name has.edges
#' @export
has.edges <- function(net, ...) {
  UseMethod("has.edges")
}

#' @export
has.edges.default <- function(net, v = seq_len(network.size(net)), ...) {
  network::has.edges(net, v, ...)
}

#' @rdname get.edgeIDs
#'
#' @title Get Edge IDs for Specified Dyads
#'
#' @param x A `networkLite` object.
#' @param v Vertex ID.
#' @param alter Vertex ID for the alter (optional). If NULL, returns incident edges.
#' @param neighborhood Specifies which edges to return when alter is NULL.
#' @param na.omit Logical; whether to exclude missing edges from the result.
#' @param ... additional arguments.
#'
#' @return The edge ID (row index in `x$el`) for the specified dyad, or
#'   numeric(0) if the edge is not present. For directed networks, the edge
#'   from `v` to `alter` is returned. For undirected networks, the edge
#'   between `v` and `alter` is returned (order does not matter).
#'
#' @details
#' Returns the edge ID for a single dyad. In networkLite, edge IDs are
#' simply row indices within `x$el`. If the edge is not present, returns
#' numeric(0). If `na.omit = TRUE`, missing edges (those with the "na"
#' attribute set to TRUE) are excluded.
#'
#' @export
#'
get.edgeIDs.networkLite <- function(x, v, alter = NULL, 
                                    neighborhood = c("out", "in", "combined"),
                                    na.omit = TRUE, ...) {
  neighborhood <- match.arg(neighborhood)
  
  v <- as.integer(v)
  
  if (is.null(alter)) {
    # Return incident edges when alter is NULL
    if (length(v) != 1) {
      stop("get.edgeIDs requires scalar v")
    }
    
    # Get edges incident on v
    if (neighborhood == "out" || neighborhood == "combined") {
      out_edges <- which(x$el$.tail %in% v)
    } else {
      out_edges <- integer(0)
    }
    
    if (neighborhood == "in" || (neighborhood == "combined" && is.directed(x))) {
      in_edges <- which(x$el$.head %in% v)
    } else if (neighborhood == "combined" && !is.directed(x)) {
      # For undirected networks, also check heads
      in_edges <- which(x$el$.head %in% v)
    } else {
      in_edges <- integer(0)
    }
    
    eids <- unique(c(out_edges, in_edges))
    
    # Filter out missing edges if na.omit is TRUE
    if (na.omit && length(eids) > 0) {
      # Check each edge's na attribute
      na_vals <- sapply(eids, function(i) isTRUE(x$el$na[[i]]))
      eids <- eids[!na_vals]
    }
    
    return(eids)
  }
  
  alter <- as.integer(alter)
  
  if (length(v) != 1 || length(alter) != 1) {
    stop("get.edgeIDs requires scalar v and alter; use get.dyads.eids for vectors")
  }
  
  if (is.na(v) || is.na(alter) || v < 1 || v > network.size(x) ||
        alter < 1 || alter > network.size(x)) {
    return(numeric(0))
  }
  
  # For undirected networks, normalize the dyad so tail < head
  if (!is.directed(x) && v > alter) {
    temp <- v
    v <- alter
    alter <- temp
  }
  
  # Find the edge in the edgelist
  eid <- which(x$el$.tail == v & x$el$.head == alter)
  
  # If na.omit is TRUE, exclude edges with na = TRUE
  if (na.omit && length(eid) > 0) {
    na_val <- x$el$na[[eid]]  # Use [[]] to get the scalar value from list
    if (isTRUE(na_val)) {
      return(numeric(0))
    }
  }
  
  return(eid)
}


#' @rdname get.dyads.eids
#'
#' @title Get Edge IDs for Multiple Dyads
#'
#' @param x A `networkLite` object.
#' @param tails Vector of tail vertex IDs.
#' @param heads Vector of head vertex IDs (must be same length as tails).
#' @param neighborhood Specifies which edges to consider.
#' @param na.omit Logical; whether to exclude missing edges from the result.
#' @param ... additional arguments.
#'
#' @return A list of edge IDs corresponding to the specified dyads. Each
#'   element is either a single edge ID or numeric(0) if the edge is not
#'   present.
#'
#' @details
#' Vectorized version of `get.edgeIDs`. Returns a list where each element
#' corresponds to the edge ID for the dyad (tails[i], heads[i]).
#'
#' @export
#'
get.dyads.eids.networkLite <- function(x, tails, heads,
                                       neighborhood = c("out", "in", "combined"),
                                       na.omit = TRUE, ...) {
  neighborhood <- match.arg(neighborhood)
  tails <- as.integer(tails)
  heads <- as.integer(heads)
  
  if (length(tails) != length(heads)) {
    stop("tails and heads must have the same length")
  }
  
  # Use lapply to get edge IDs for each dyad
  eids <- lapply(seq_along(tails), function(i) {
    get.edgeIDs(x, tails[i], heads[i], neighborhood = neighborhood, na.omit = na.omit)
  })
  
  return(eids)
}


#' @rdname get.edges
#'
#' @title Get Edges
#'
#' @param x A `networkLite` object.
#' @param v,alter Vertex IDs. If both are provided, returns edges between
#'   v and alter. If only v is provided, returns edges incident on v.
#' @param neighborhood Specifies which edges to return: "out" for outgoing
#'   edges, "in" for incoming edges, "combined" for both.
#' @param na.omit Logical; whether to exclude missing edges from the result.
#' @param ... additional arguments.
#'
#' @return A vector of edge IDs.
#'
#' @details
#' Returns edge IDs based on the vertex selection. If both `v` and `alter`
#' are specified, returns edges between those vertices. If only `v` is
#' specified, returns edges incident on `v` according to the `neighborhood`
#' parameter.
#'
#' @export
#'
get.edges.networkLite <- function(x, v, alter, neighborhood = c("combined", "out", "in"),
                                  na.omit = TRUE, ...) {
  neighborhood <- match.arg(neighborhood)
  
  if (!missing(alter)) {
    # Get edges between v and alter
    return(unlist(get.dyads.eids(x, v, alter, na.omit = na.omit)))
  }
  
  v <- as.integer(v)
  
  # Get edges incident on v
  if (neighborhood == "out" || neighborhood == "combined") {
    out_edges <- which(x$el$.tail %in% v)
  } else {
    out_edges <- integer(0)
  }
  
  if (neighborhood == "in" || (neighborhood == "combined" && is.directed(x))) {
    in_edges <- which(x$el$.head %in% v)
  } else if (neighborhood == "combined" && !is.directed(x)) {
    # For undirected networks, also check heads
    in_edges <- which(x$el$.head %in% v)
  } else {
    in_edges <- integer(0)
  }
  
  eids <- unique(c(out_edges, in_edges))
  
  # Filter out missing edges if na.omit is TRUE
  if (na.omit && length(eids) > 0) {
    # Check each edge's na attribute
    na_vals <- sapply(eids, function(i) isTRUE(x$el$na[[i]]))
    eids <- eids[!na_vals]
  }
  
  return(eids)
}


#' @rdname get.neighborhood
#'
#' @title Get Neighborhood of Vertices
#'
#' @param x A `networkLite` object.
#' @param v Vertex ID or vector of vertex IDs.
#' @param type Specifies which neighbors to return: "out" for out-neighbors,
#'   "in" for in-neighbors, "combined" for both.
#' @param na.omit Logical; whether to exclude neighbors connected by missing edges.
#' @param ... additional arguments.
#'
#' @return A vector of vertex IDs representing the neighborhood of v.
#'
#' @details
#' Returns the neighborhood (adjacent vertices) of the specified vertex or
#' vertices. For directed networks, the type parameter controls whether
#' out-neighbors, in-neighbors, or both are returned.
#'
#' @export
#'
get.neighborhood.networkLite <- function(x, v, type = c("combined", "out", "in"),
                                         na.omit = TRUE, ...) {
  type <- match.arg(type)
  v <- as.integer(v)
  
  neighbors <- integer(0)
  
  # Get out-neighbors (vertices that v points to)
  if (type == "out" || type == "combined") {
    out_idx <- which(x$el$.tail %in% v)
    if (na.omit) {
      # Filter out missing edges
      na_vals <- sapply(out_idx, function(i) isTRUE(x$el$na[[i]]))
      out_idx <- out_idx[!na_vals]
    }
    neighbors <- c(neighbors, x$el$.head[out_idx])
  }
  
  # Get in-neighbors (vertices that point to v)
  if (type == "in" || type == "combined") {
    in_idx <- which(x$el$.head %in% v)
    if (na.omit) {
      # Filter out missing edges
      na_vals <- sapply(in_idx, function(i) isTRUE(x$el$na[[i]]))
      in_idx <- in_idx[!na_vals]
    }
    neighbors <- c(neighbors, x$el$.tail[in_idx])
  }
  
  # Return unique neighbors, excluding v itself
  unique(setdiff(neighbors, v))
}


#' @rdname is.adjacent
#'
#' @title Test for Edge Existence
#'
#' @param x A `networkLite` object.
#' @param vi,vj Vertex IDs.
#' @param na.omit Logical; whether to treat missing edges as non-existent.
#'   Default is FALSE to match network package behavior.
#' @param ... additional arguments.
#'
#' @return Logical indicating whether an edge exists from vi to vj (or
#'   between vi and vj for undirected networks).
#'
#' @details
#' Tests whether an edge exists between the specified vertices. For directed
#' networks, tests for an edge from vi to vj. For undirected networks, tests
#' for an edge between vi and vj (order does not matter). Note that the
#' default for na.omit is FALSE, meaning missing edges are treated as
#' present by default (consistent with network package behavior).
#'
#' @export
#'
is.adjacent.networkLite <- function(x, vi, vj, na.omit = FALSE, ...) {
  # Explicitly pass na.omit to get.edgeIDs
  # Note: get.edgeIDs has default na.omit=TRUE, but we override it here
  eid <- get.edgeIDs(x, vi, vj, na.omit = na.omit)
  length(eid) > 0
}


#' @rdname has.edges
#'
#' @title Test for Edge Existence in Network
#'
#' @param net A `networkLite` object.
#' @param v Vertex IDs to check for incident edges. Defaults to all vertices.
#' @param ... additional arguments.
#'
#' @return Logical indicating whether the specified vertices have any incident edges.
#'
#' @details
#' Returns TRUE if any of the specified vertices have at least one incident edge 
#' (excluding missing edges), FALSE otherwise.
#'
#' @export
#'
has.edges.networkLite <- function(net, v = seq_len(network.size(net)), ...) {
  if (length(v) == 0 || network.edgecount(net, na.omit = TRUE) == 0) {
    return(FALSE)
  }
  
  v <- as.integer(v)
  
  # Check if any edges involve the specified vertices
  if (length(v) > 0 && network.edgecount(net, na.omit = TRUE) > 0) {
    edge_indices <- which(net$el$.tail %in% v | net$el$.head %in% v)
    if (length(edge_indices) > 0) {
      # Check if any of these edges are not missing
      na_vals <- sapply(edge_indices, function(i) isTRUE(net$el$na[[i]]))
      return(any(!na_vals))
    }
  }
  
  return(FALSE)
}
