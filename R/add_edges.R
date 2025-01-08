
#' @rdname add_edges
#'
#' @title Methods to Add or Modify Edges in a `networkLite`.
#'
#' @param x A `networkLite`.
#' @param tail Vector of tails of edges to add to the `networkLite`.
#' @param head Vector of heads of edges to add to the `networkLite`.
#' @param names.eval Names of edge attributes, or `NULL` to indicate that
#'        attributes are not being specified. For `add.edges`, this
#'        argument should be structured as a list of length equal to
#'        `length(tail)`, each element of which is a character vector
#'        of attribute names for the corresponding edge. For the replacement
#'        method `[<-.networkLite`, this should argument should be a
#'        single attribute name, which is applied to all edges.
#' @param vals.eval Value(s) of edge attributes, or `NULL` to indicate
#'        that attributes are not being specified. This argument should be
#'        structured as a list of length equal to `length(tail)`, each
#'        element of which is a list of attribute values, in the same order
#'        as the corresponding attribute names in `names.eval`.
#' @param i,j Nodal indices (must be missing for `networkLite` method).
#' @param add.edges logical; should edges being assigned to be added if they
#'        are not already present?
#' @param value Edge values to assign (coerced to a matrix).
#' @param ... additional arguments
#'
#' @return
#' A `networkLite` object with edges added (if calling
#' `add.edges`) or set to specified values (if calling
#' `[<-.networkLite`).
#'
#' @export
#'
add.edges.networkLite <- function(x, tail, head, names.eval = NULL,
                                  vals.eval = NULL, ...) {
  ## convert to atomic...
  tail <- NVL(as.integer(unlist(tail)), integer(0))
  head <- NVL(as.integer(unlist(head)), integer(0))

  ## if we were passed any attribute information...
  if (length(unlist(names.eval))  > 0) {
    if (!is.list(names.eval)) names.eval <-
      as.list(rep(names.eval, length.out = length(tail)))
    if (!is.list(vals.eval)) vals.eval <-
      as.list(rep(vals.eval, length.out = length(names.eval)))

    for (i in seq_along(vals.eval)) {
      vals.eval[[i]] <- as.list(vals.eval[[i]])
      names(vals.eval[[i]]) <- names.eval[[i]]
    }

    new_names <- unique(unlist(names.eval))
    update_list <- lapply(new_names, function(name) lapply(vals.eval, `[[`, name))
    names(update_list) <- new_names
  } else {
    update_list <- list()
  }

  if ("na" %in% names(update_list)) {
    update_list[["na"]] <- lapply(update_list[["na"]], isTRUE)
  } else {
    update_list <- c(update_list, list(na = logical(length(tail))))
  }
  update_tibble <- as_tibble(c(list(.tail = tail, .head = head), update_list))

  new_names <- names(update_tibble) # including ".tail", ".head", and "na"
  old_names <- names(x$el)

  for (name in setdiff(old_names, new_names)) {
    update_tibble[[name]] <- vector(mode = "list", length = NROW(update_tibble))
  }

  for (name in setdiff(new_names, old_names)) {
    x$el[[name]] <- vector(mode = "list", length = NROW(x$el))
  }

  x$el <- dplyr::bind_rows(ensure_list(list(x$el, update_tibble)))
  x$el <- x$el[order(x$el$.tail, x$el$.head), ]
  x$el <- x$el[!duplicated(x$el[, c(".tail", ".head")]), ]

  modify_in_place(x)
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
    x$el <- as_tibble(list(.tail = as.integer(w[, 1]),
                           .head = as.integer(w[, 2]),
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
        tbl_list <- list(as.integer(w[, 1]), as.integer(w[, 2]), vals)
        names(tbl_list) <- c(".tail",
                             ".head",
                             names.eval)
      } else {
        tbl_list <- list(as.integer(w[, 1]), as.integer(w[, 2]), vals, logical(NROW(w)))
        names(tbl_list) <- c(".tail", ".head", names.eval, "na")
      }
      x$el <- as_tibble(tbl_list)
    }
  }
  return(x)
}
