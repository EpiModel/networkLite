
#' @rdname edgecount
#' @title Count Edges in a \code{networkLite}
#' @param x A \code{networkLite} object.
#' @param na.omit logical; omit missing edges from edge count?
#' @param ... additional arguments
#' @details The \code{network.edgecount} method provides a count of the number
#'          of edges in the \code{networkLite}, including missing edges if
#'          \code{na.omit = FALSE} and omitting them if \code{na.omit = TRUE}.
#'          The \code{network.naedgecount} method provides a count of the
#'          number of missing edges in the \code{networkLite}.
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
#' @title Print Basic Summary of a \code{networkLite}
#' @param x A \code{networkLite} object.
#' @param ... additional arguments
#' @details This method prints a basic summary of a \code{networkLite} object,
#'          including network size, edge count, and attribute names.
#' @export
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
#' @title Extract \code{networkLite} with Missing Edges Only
#' @param x A \code{networkLite}.
#' @details The \code{is.na} method creates a \code{networkLite} whose edges
#'          are precisely those edges in \code{x} that are missing.  The edges
#'          in the return value are marked as not missing.
#' @export
is.na.networkLite <- function(x) {
  y <- networkLite(network.size(x),
                   directed = x %n% "directed",
                   bipartite = x %n% "bipartite")
  el <- as.edgelist(x, na.rm = FALSE)
  elna <- el[NVL(x %e% "na", FALSE), , drop = FALSE]
  add.edges(y, elna[, 1], elna[, 2])
  y
}

#' @rdname operators
#' @title Add and Subtract \code{networkLite}s
#' @param e1,e2 \code{networkLite} objects
#' @details \code{e1 + e2} produces a \code{networkLite} whose edges are those
#'          in either \code{e1} or \code{e2}, and \code{e1 - e2} produces a
#'          \code{networkLite} whose edges are those in \code{e1} and not in
#'          \code{e2}.
#' @export
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

  out <- e1
  if (network.edgecount(e2, na.omit = FALSE) > 0) {
    edgelist <- dplyr::bind_rows(ensure_list(list(e1$el, e2$el)))
    edgelist <- edgelist[!duplicated(edgelist[, c(".tail", ".head")]), ]
    out$el <- edgelist[order(edgelist$.tail, edgelist$.head), ]
  }
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

  out <- e1
  if (network.edgecount(e2, na.omit = FALSE) > 0) {
    edgelist <- dplyr::bind_rows(ensure_list(list(e2$el, e1$el)))
    nd <- !duplicated(edgelist[, c(".tail", ".head")])
    out$el <- out$el[nd[-seq_len(network.edgecount(e2, na.omit = FALSE))], ]
    out$el <- out$el[order(out$el$.tail, out$el$.head), ]
  }
  out
}

# x = a list of tibbles
ensure_list <- function(x) {
  names <- sort(unique(unlist(lapply(x, names))))
  for(name in names) {
    any_list <- any(unlist(lapply(lapply(x, `[[`, name), is.list)))
    if (any_list == TRUE) {
      x <- lapply(x, function(y) { if (name %in% names(y)) { y[[name]] <- as.list(y[[name]]) }; y })
    }
  }
  return(x)
}
