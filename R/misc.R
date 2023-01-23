
#' @rdname edgecount
#'
#' @title Count Edges in a \code{networkLite}
#'
#' @param x A \code{networkLite} object.
#' @param na.omit logical; omit missing edges from edge count?
#' @param ... additional arguments
#'
#' @return The number of edges (of the appropriate type) in \code{x}.
#'
#' @details The \code{network.edgecount} method provides a count of the number
#' of edges in the \code{networkLite}, including missing edges if
#' \code{na.omit = FALSE} and omitting them if \code{na.omit = TRUE}.
#' The \code{network.naedgecount} method provides a count of the
#' number of missing edges in the \code{networkLite}.
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
#' @title Print Basic Summary of a \code{networkLite}
#'
#' @param x A \code{networkLite} object.
#' @param ... additional arguments
#'
#' @return The \code{networkLite} is returned invisibly.
#'
#' @details
#' This method prints a basic summary of a \code{networkLite} object,
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
#' @title Extract \code{networkLite} with Missing Edges Only
#'
#' @param x A \code{networkLite}.
#'
#' @return
#' A \code{networkLite} with the same network size, directedness, and
#' bipartiteness as \code{x}, whose edges are precisely those edges in
#' \code{x} that are missing in \code{x}. Edges in the returned
#' \code{networkLite} are marked as not missing.
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
#' @title Add and Subtract \code{networkLite}s
#'
#' @param e1,e2 \code{networkLite} objects
#' @return
#' For the \code{+} method, a \code{networkLite} whose edges are those
#' in either \code{e1} or \code{e2}. For the \code{-} method, a
#' \code{networkLite} whose edges are those in \code{e1} and not in
#' \code{e2}.
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
  out <- add.edges(out, edgelist$.tail, edgelist$.head)
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
  out <- add.edges(out, edgelist$.tail, edgelist$.head)
  out
}

#' @rdname valid.eids
#'
#' @title valid.eids
#'
#' @param x A \code{networkLite} object.
#' @param ... additional arguments.
#'
#' @return
#' The sequence \code{seq_len(network.edgecount(x, na.omit = FALSE))}.
#'
#' @details
#' Returns \code{seq_len(network.edgecount(x, na.omit = FALSE))}, to
#' support the edge attribute assignment operator \code{\%e\%<-}. Note
#' that the edge id of an edge in \code{x} is simply its row index
#' within \code{x$el}.
#'
#' @export
#'
valid.eids.networkLite <- function(x, ...) {
  seq_len(network.edgecount(x, na.omit = FALSE))
}
