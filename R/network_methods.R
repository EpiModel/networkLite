
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
  
  if (is.null(alter)) {
    # Return incident edges when alter is NULL
    return(get.edges(x, v, neighborhood = neighborhood, na.omit = na.omit))
  }
  
  v <- as.integer(v)
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
  if (na.omit && length(eid) > 0 && isTRUE(x$el$na[eid])) {
    return(numeric(0))
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
    eids <- eids[!NVL(x$el$na[eids], FALSE)]
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
      out_idx <- out_idx[!NVL(x$el$na[out_idx], FALSE)]
    }
    neighbors <- c(neighbors, x$el$.head[out_idx])
  }
  
  # Get in-neighbors (vertices that point to v)
  if (type == "in" || type == "combined") {
    in_idx <- which(x$el$.head %in% v)
    if (na.omit) {
      in_idx <- in_idx[!NVL(x$el$na[in_idx], FALSE)]
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
#' @param ... additional arguments.
#'
#' @return Logical indicating whether an edge exists from vi to vj (or
#'   between vi and vj for undirected networks).
#'
#' @details
#' Tests whether an edge exists between the specified vertices. For directed
#' networks, tests for an edge from vi to vj. For undirected networks, tests
#' for an edge between vi and vj (order does not matter).
#'
#' @export
#'
is.adjacent.networkLite <- function(x, vi, vj, na.omit = FALSE, ...) {
  eid <- get.edgeIDs(x, vi, vj, na.omit = na.omit)
  length(eid) > 0
}


#' @rdname network.density
#'
#' @title Calculate Network Density
#'
#' @param x A `networkLite` object.
#' @param na.omit Logical; whether to exclude missing edges from the calculation.
#' @param discount.bipartite Logical; for bipartite networks, whether to compute
#'   density based on within-mode edges (if FALSE) or only between-mode edges (if TRUE).
#' @param ... additional arguments.
#'
#' @return The network density (proportion of possible edges that are present).
#'
#' @details
#' Calculates the density of the network as the ratio of the number of edges
#' to the number of possible edges. For directed networks, the number of
#' possible edges is n*(n-1). For undirected networks, it is n*(n-1)/2,
#' where n is the network size. For bipartite networks, the number of
#' possible edges is n1*n2 when discount.bipartite = FALSE, where n1 and n2 
#' are the sizes of the two modes.
#'
#' @export
#'
network.density.networkLite <- function(x, na.omit = TRUE, discount.bipartite = FALSE, ...) {
  n <- network.size(x)
  
  if (n == 0) {
    return(NaN)
  }
  
  edge_count <- network.edgecount(x, na.omit = na.omit)
  
  if (is.bipartite(x) && !discount.bipartite) {
    b1 <- x %n% "bipartite"
    b2 <- n - b1
    max_edges <- b1 * b2
  } else if (is.directed(x)) {
    max_edges <- n * (n - 1)
  } else {
    max_edges <- n * (n - 1) / 2
  }
  
  if (max_edges == 0) {
    return(NaN)
  }
  
  edge_count / max_edges
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
  any(net$el$.tail %in% v | net$el$.head %in% v) &&
    any(!NVL(net$el$na[net$el$.tail %in% v | net$el$.head %in% v], FALSE))
}
