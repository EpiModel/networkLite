
#' @rdname edgecount
#'
#' @title Count Edges in a `networkLite`
#'
#' @param x A `networkLite` object.
#' @param na.omit logical; omit missing edges from edge count?
#' @param ... additional arguments
#'
#' @return The number of edges (of the appropriate type) in `x`.
#'
#' @details The `network.edgecount` method provides a count of the number
#' of edges in the `networkLite`, including missing edges if
#' `na.omit = FALSE` and omitting them if `na.omit = TRUE`.
#' The `network.naedgecount` method provides a count of the
#' number of missing edges in the `networkLite`.
#'
#' @export
#'
network.edgecount.networkLite <- function(x, na.omit = TRUE, ...) {
  if (na.omit == TRUE) {
    NROW(x$el) - network.naedgecount(x)
  } else {
    NROW(x$el)
  }
}

#' @rdname edgecount
#' @export
network.naedgecount.networkLite <- function(x, ...) {
  sum(x %e% "na")
}

#' @rdname print
#'
#' @title Print Basic Summary of a `networkLite`
#'
#' @param x A `networkLite` object.
#' @param ... additional arguments
#'
#' @return The `networkLite` is returned invisibly.
#'
#' @details
#' This method prints a basic summary of a `networkLite` object,
#' including network size, edge count, and attribute names.
#'
#' @export
#'
print.networkLite <- function(x, ...) {
  cat("networkLite with properties:\n")
  cat("  Network size:", network.size(x), "\n")
  cat("  Edge count:", network.edgecount(x, na.omit = FALSE), "\n")
  cat("    Non-missing edge count:", network.edgecount(x, na.omit = TRUE), "\n")
  cat("    Missing edge count:", network.naedgecount(x), "\n")
  cat("  Network attributes:", list.network.attributes(x), "\n")
  cat("  Vertex attributes:", list.vertex.attributes(x), "\n")
  cat("  Edge attributes:", list.edge.attributes(x), "\n")
  invisible(x)
}

#' @rdname is.na
#'
#' @title Extract `networkLite` with Missing Edges Only
#'
#' @param x A `networkLite`.
#'
#' @return
#' A `networkLite` with the same network size, directedness, and
#' bipartiteness as `x`, whose edges are precisely those edges in
#' `x` that are missing in `x`. Edges in the returned
#' `networkLite` are marked as not missing.
#'
#' @export
#'
is.na.networkLite <- function(x) {
  y <- networkLite(network.size(x),
                   directed = x %n% "directed",
                   bipartite = x %n% "bipartite")
  el <- as.edgelist(x, na.rm = FALSE)
  elna <- el[NVL(x %e% "na", logical(NROW(el))), , drop = FALSE]
  add.edges(y, elna[, 1], elna[, 2])
  y
}

#' @rdname operators
#'
#' @title Add and Subtract `networkLite`s
#'
#' @param e1,e2 `networkLite` objects
#' @return
#' For the `+` method, a `networkLite` whose edges are those
#' in either `e1` or `e2`. For the `-` method, a
#' `networkLite` whose edges are those in `e1` and not in
#' `e2`.
#'
#' @export
#'
`+.networkLite` <- function(e1, e2) {
  if (!identical(e1 %n% "n", e2 %n% "n") ||
        !identical(e1 %n% "directed", e2 %n% "directed") ||
        !identical(e1 %n% "bipartite", e2 %n% "bipartite")) {
    stop("cannot add networkLites of differing network size, directedness, or",
         " bipartiteness")
  }

  if (any(NVL(e1 %e% "na", FALSE)) || any(NVL(e2 %e% "na", FALSE))) {
    stop("adding networkLites with missing edges is not currently supported")
  }

  if (network.edgecount(e2, na.omit = FALSE) > 0) {
    edgelist <- tibble(.tail = c(e1$el$.tail, e2$el$.tail),
                       .head = c(e1$el$.head, e2$el$.head))
    edgelist <- edgelist[!duplicated(edgelist), ]
    edgelist <- edgelist[order(edgelist$.tail, edgelist$.head), ]
  } else {
    edgelist <- tibble(.tail = e1$el$.tail, .head = e1$el$.head)
  }
  out <- networkLite(e1 %n% "n", e1 %n% "directed", e1 %n% "bipartite")
  add.edges(out, edgelist$.tail, edgelist$.head)
  out
}

#' @rdname operators
#' @export
`-.networkLite` <- function(e1, e2) {
  if (!identical(e1 %n% "n", e2 %n% "n") ||
        !identical(e1 %n% "directed", e2 %n% "directed") ||
        !identical(e1 %n% "bipartite", e2 %n% "bipartite")) {
    stop("cannot subtract networkLites of differing network size,",
         " directedness, or bipartiteness")
  }

  if (any(NVL(e1 %e% "na", FALSE)) || any(NVL(e2 %e% "na", FALSE))) {
    stop("subtracting networkLites with missing edges is not currently",
         " supported")
  }

  if (network.edgecount(e2, na.omit = FALSE) > 0) {
    edgelist <- tibble(.tail = c(e2$el$.tail, e1$el$.tail),
                       .head = c(e2$el$.head, e1$el$.head))
    nd <- !duplicated(edgelist)
    edgelist <- e1$el[nd[-seq_len(network.edgecount(e2, na.omit = FALSE))], ]
    edgelist <- edgelist[order(edgelist$.tail, edgelist$.head), ]
  } else {
    edgelist <- tibble(.tail = e1$el$.tail, .head = e1$el$.head)
  }
  out <- networkLite(e1 %n% "n", e1 %n% "directed", e1 %n% "bipartite")
  add.edges(out, edgelist$.tail, edgelist$.head)
  out
}

#' @rdname valid.eids
#'
#' @title valid.eids
#'
#' @param x A `networkLite` object.
#' @param ... additional arguments.
#'
#' @return
#' The sequence `seq_len(network.edgecount(x, na.omit = FALSE))`.
#'
#' @details
#' Returns `seq_len(network.edgecount(x, na.omit = FALSE))`, to
#' support the edge attribute assignment operator `\%e\%<-`. Note
#' that the edge id of an edge in `x` is simply its row index
#' within `x$el`.
#'
#' @export
#'
valid.eids.networkLite <- function(x, ...) {
  seq_len(network.edgecount(x, na.omit = FALSE))
}
