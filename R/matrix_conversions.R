#' @rdname matrix_conversions
#' @title Convert a \code{networkLite} to a Matrix or \code{tibble}.
#' @param x A \code{networkLite}.
#' @param attrname Name of an edge attribute in \code{x}.
#' @param attrnames Vector specifying edge attributes to include in the tibble;
#'        may be logical, integer, or character vector, the former two being
#'        used to select attribute names from \code{list.edge.attributes(x)},
#'        and the latter being used as the attribute names themselves
#' @param output Type of edgelist to output.
#' @param na.rm should missing edges be dropped from edgelist?
#' @param matrix.type type of matrix to return from
#'        \code{as.matrix.networkLite}
#' @param ... additional arguments
#' @export
#'
as.edgelist.networkLite <- function(x, attrname = NULL,
                                    output = c("matrix", "tibble"),
                                    na.rm = TRUE, ...) {
  output <- match.arg(output)

  if (output == "matrix") {
    m <- matrix(c(x$el$.tail, x$el$.head), ncol = 2)
    if (!is.null(attrname)) {
      m <- cbind(m, get.edge.attribute(x, attrname, null.na = TRUE, unlist = TRUE))
    }
  } else {
    m <- x$el[c(".tail", ".head", attrname)]
  }

  if (na.rm && NROW(m) > 0) {
    na <- NVL(x %e% "na", FALSE)
    m <- m[!na, , drop = FALSE]
  }

  if (output == "tibble") {
    m <- atomize_tibble(m)
  }
  attr(m, "dimnames") <- NULL

  attr(m, "n") <- as.integer(network.size(x))
  attr(m, "vnames") <- network.vertex.names(x)
  bip <- if (is.bipartite(x)) x %n% "bipartite" else FALSE
  attr(m, "bipartite") <- if (is.numeric(bip)) as.integer(bip) else bip
  attr(m, "directed") <- as.logical(is.directed(x))
  attr(m, "loops") <- as.logical(has.loops(x))
  class(m) <- c(if (output == "matrix") "matrix_edgelist" else "tibble_edgelist",
                "edgelist", class(m))
  return(m)
}


#' @rdname matrix_conversions
#' @export
as_tibble.networkLite <- function(x, attrnames = NULL, na.rm = TRUE, ...) {
  if (is.logical(attrnames) || is.numeric(attrnames))
    attrnames <- na.omit(list.edge.attributes(x)[attrnames])
  out <- x$el[, c(".tail", ".head", attrnames)]
  if (na.rm && NROW(out) > 0) {
    na <- NVL(x %e% "na", FALSE)
    out <- out[!na, ]
  }
  out <- atomize_tibble(out)
  attr(out, "n") <- network.size(x)
  attr(out, "vnames") <- network.vertex.names(x)
  if (is.bipartite(x)) attr(out, "bipartite") <- x %n% "bipartite"
  out
}

#' @rdname matrix_conversions
#' @export
as.matrix.networkLite <- function(x,
                                  matrix.type = c("adjacency",
                                                  "incidence", "edgelist"),
                                  attrname = NULL, ...) {
  matrix.type <- match.arg(matrix.type)
  switch(matrix.type,
         adjacency = as.matrix.networkLite.adjacency(x, attrname, ...),
         incidence = as.matrix.networkLite.incidence(x, attrname, ...),
         edgelist = as.matrix.networkLite.edgelist(x, attrname, ...))
}

as.matrix.networkLite.adjacency <- function(x, attrname = NULL, ...) {
  el <- as.edgelist(x, na.rm = FALSE)

  if (!is.null(attrname)) {
    vals <- x %e% attrname
  } else {
    vals <- rep(1, network.edgecount(x, na.omit = FALSE))
  }
  vals[NVL(x %e% "na", FALSE)] <- NA

  n <- network.size(x)

  m <- matrix(0, nrow = n, ncol = n)
  m[el] <- vals
  if (!is.directed(x)) {
    m[el[, c(2, 1)]] <- vals
  }
  dimnames(m) <- rep(list(network.vertex.names(x)), 2)

  if (is.bipartite(x)) {
    bip <- x %n% "bipartite"
    m[seq_len(bip), -seq_len(bip)]
  } else {
    m
  }
}

as.matrix.networkLite.incidence <- function(x, attrname = NULL, ...) {
  el <- as.edgelist(x, na.rm = FALSE)

  vals <- NVL2(attrname, x %e% attrname,
               rep(1, network.edgecount(x, na.omit = FALSE)))
  vals[NVL(x %e% "na", FALSE)] <- NA

  m <- matrix(0, nrow = network.size(x),
              ncol = network.edgecount(x, na.omit = FALSE))

  m[cbind(el[, 1], seq_len(NROW(el)))] <- if (is.directed(x)) -vals else vals
  m[cbind(el[, 2], seq_len(NROW(el)))] <- vals

  m
}

as.matrix.networkLite.edgelist <- function(x, attrname = NULL,
                                           na.rm = TRUE, ...) {

  m <- matrix(c(x$el$.tail, x$el$.head), ncol = 2)
  if (!is.null(attrname)) {
    m <- cbind(m, get.edge.attribute(x, attrname))
  }
  if (na.rm == TRUE) {
    m <- m[!NVL(x %e% "na", FALSE), , drop = FALSE]
  }
  attr(m, "n") <- network.size(x)
  attr(m, "vnames") <- network.vertex.names(x)
  if (is.bipartite(x)) attr(m, "bipartite") <- x %n% "bipartite"
  m
}
