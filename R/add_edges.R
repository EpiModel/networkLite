
#' @rdname add_edges
#' @title Methods to Add or Modify Edges in a \code{networkLite}
#' @param x A \code{networkLite}.
#' @param tail Vector of tails of edges to add to the networkLite.
#' @param head Vector of heads of edges to add to the networkLite.
#' @param names.eval name(s) of edge attributes
#' @param vals.eval value(s) of edge attributes
#' @param i,j Nodal indices (must be missing for networkLite method).
#' @param add.edges should edges being assigned to be added if not already
#'        present?
#' @param value edge values to assign
#' @param ... additional arguments
#' @export
add.edges.networkLite <- function(x, tail, head, names.eval = NULL,
                                  vals.eval = NULL, ...) {
  tail <- NVL(unlist(tail), integer(0))
  head <- NVL(unlist(head), integer(0))
  if (length(names.eval) == 0 || length(vals.eval) == 0) {
    update_tibble <- as_tibble(list(.tail = tail, .head = head,
                                    na = logical(length(tail))))
  } else {
    if (!is.list(names.eval)) names.eval <-
        as.list(rep(names.eval, length.out = length(tail)))
    if (!is.list(vals.eval)) vals.eval <-
        as.list(rep(vals.eval, length.out = length(names.eval)))

    for (i in seq_along(vals.eval)) {
      vals.eval[[i]] <- as.list(vals.eval[[i]])
      names(vals.eval[[i]]) <- unlist(names.eval[[i]])
    }

    f <- function(x) if (length(x) > 0) as_tibble(x) else tibble(NULL, .rows = 1)
    update_tibble <-
      dplyr::bind_cols(as_tibble(list(.tail = tail, .head = head)),
                        dplyr::bind_rows(lapply(vals.eval, f)))
  }

  update_tibble[["na"]] <- NVL(update_tibble[["na"]],
                               logical(NROW(update_tibble)))
  update_tibble[["na"]][is.na(update_tibble[["na"]])] <- FALSE

  xn <- substitute(x)

  x$el <- dplyr::bind_rows(x$el, update_tibble)
  x$el <- x$el[order(x$el$.tail, x$el$.head), ]
  x$el <- x$el[!duplicated(x$el[, c(".tail", ".head")]), ]

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}

#' @rdname add_edges
#' @export
"[<-.networkLite" <- function(x, i, j, names.eval = NULL,
                              add.edges = FALSE, value) {
  if (!missing(i) || !missing(j)) {
    stop("`[<-.networkLite` does not support `i` and `j` arguments at this",
         " time")
  }

  if (any(is.na(value))) {
    stop("`[<-.networkLite` does not support NA `value` arguments at this",
         " time")
  }

  if (is.null(names.eval) && isTRUE(all(value == FALSE))) {
    x$el <- as_tibble(list(.tail = integer(0),
                           .head = integer(0),
                           na = logical(0)))
    return(x)
  }

  b1 <- if (is.bipartite(x)) x %n% "bipartite" else
    network.size(x)
  b2 <- if (is.bipartite(x)) network.size(x) - x %n% "bipartite" else
    network.size(x)

  if (!is.matrix(value)) {
    value <- matrix(value, nrow = b1, ncol = b2)
  } else {
    if (nrow(value) < b1 || ncol(value) < b2) {
      stop("too small a matrix `value` passed to `[<-.networkLite`")
    }
    value <- value[seq_len(b1), seq_len(b2), drop = FALSE]
  }

  if (is.null(names.eval)) {
    # add edges whether or not add.edges is TRUE,
    #   for consistency with `network` behavior
    w <- which(value != 0, arr.ind = TRUE)
    if (is.bipartite(x)) {
      w[, 2] <- w[, 2] + b1
    }
    if (!is.directed(x)) {
      w <- w[w[, 1] < w[, 2], , drop = FALSE]
    } else {
      w <- w[w[, 1] != w[, 2], , drop = FALSE]
    }
    w <- w[order(w[, 1], w[, 2]), , drop = FALSE]
    x$el <- as_tibble(list(.tail = w[, 1],
                           .head = w[, 2],
                           na = logical(NROW(w))))
  } else {
    if (!add.edges) {
      el <- as.edgelist(x, na.rm = FALSE)
      if (is.bipartite(x)) {
        el[, 2] <- el[, 2] - b1
      }
      if (names.eval == "na") {
        value[is.na(value)] <- FALSE
      }
      set.edge.attribute(x, names.eval, value[el])
    } else {
      w <- which(value != 0, arr.ind = TRUE)
      vals <- value[w]
      if (is.bipartite(x)) {
        w[, 2] <- w[, 2] + b1
      }
      if (!is.directed(x)) {
        vals <- vals[w[, 1] < w[, 2]]
        w <- w[w[, 1] < w[, 2], , drop = FALSE]
      } else {
        vals <- vals[w[, 1] != w[, 2]]
        w <- w[w[, 1] != w[, 2], , drop = FALSE]
      }
      vals <- vals[order(w[, 1], w[, 2])]
      w <- w[order(w[, 1], w[, 2]), , drop = FALSE]
      if (names.eval == "na") {
        vals[is.na(vals)] <- FALSE
        tbl_list <- list(w[, 1], w[, 2], vals)
        names(tbl_list) <- c(".tail",
                             ".head",
                             names.eval)
      } else {
        tbl_list <- list(w[, 1], w[, 2], vals, logical(NROW(w)))
        names(tbl_list) <- c(".tail", ".head", names.eval, "na")
      }
      x$el <- as_tibble(tbl_list)
    }
  }
  return(x)
}
