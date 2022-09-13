
#' @rdname attribute_methods
#' @title \code{networkLite} Attribute Methods
#'
#' @description S3 attribute methods for the \code{networkLite} class, for
#'              generics defined in the \code{network} package.
#'
#' @param x A \code{networkLite} object.
#' @param attrname The name of an attribute in \code{x}; must be a length one
#'        character vector.
#' @param value The attribute value to set in vertex, edge, and network
#'        attribute setters.
#' @param v Indices at which to set vertex attribute values.
#' @param e Indices at which to set edge attribute values.
#' @param null.na Logical. If \code{TRUE}, replace \code{NULL} attribute values
#'        with \code{NA} in \code{get.vertex.attribute} and
#'        \code{get.edge.attribute}. Applied before the \code{unlist} argument.
#' @param unlist Logical. In \code{get.vertex.attribute} and
#'        \code{get.edge.attribute}, if \code{unlist} is \code{TRUE}, we call
#'        \code{unlist} on the attribute value before returning it, and if
#'        \code{unlist} is \code{FALSE}, we call \code{as.list} on the
#'        attribute value before returning it. In \code{get.network.attribute},
#'        if \code{unlist} is \code{TRUE}, we call \code{unlist} on the
#'        attribute value before returning it, and if \code{unlist} is
#'        \code{FALSE}, we return the attribute value without any modification.
#' @param ... additional arguments
#'
#' @details Allows basic attribute manipulation for \code{networkLite}s.
#'
#' @return Behavior and return values are analogous to those of the
#'         corresponding \code{network} methods, with network data structured
#'         in the \code{networkLite} format.
#'
#' @export
#'
get.vertex.attribute.networkLite <- function(x, attrname, ..., null.na = TRUE, unlist = TRUE) {
  if (attrname %in% list.vertex.attributes(x)) {
    out <- x$attr[[attrname]]
  } else {
    out <- vector(mode = "list", length = network.size(x))
  }

  if (null.na == TRUE && is.list(out)) {
    out <- lapply(out, NVL, NA)
  }

  if (unlist == TRUE) {
    out <- unlist(out)
  } else {
    out <- as.list(out)
  }

  return(out)
}

#' @rdname attribute_methods
#' @export
#'
set.vertex.attribute.networkLite <- function(x,
                                             attrname,
                                             value,
                                             v = seq_len(network.size(x)),
                                             ...) {
  xn <- substitute(x)

  if (!(attrname %in% list.vertex.attributes(x))) {
    x$attr[[attrname]] <- vector(mode = "list", length = network.size(x))
  }

  x$attr[[attrname]][v] <- value

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}

#' @rdname attribute_methods
#' @export
#'
list.vertex.attributes.networkLite <- function(x, ...) {
  sort(unique(names(x$attr)))
}

#' @rdname attribute_methods
#' @export
#'
get.network.attribute.networkLite <- function(x, attrname, ..., unlist = FALSE) {
  out <- x$gal[[attrname]]

  if (unlist == TRUE) {
    out <- unlist(out)
  }

  return(out)
}

#' @rdname attribute_methods
#' @export
#'
set.network.attribute.networkLite <- function(x, attrname, value, ...) {
  xn <- substitute(x)

  x$gal[[attrname]] <- value

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}

#' @rdname attribute_methods
#' @export
#'
list.network.attributes.networkLite <- function(x, ...) {
  sort(unique(names(x$gal)))
}

#' @rdname attribute_methods
#' @export
#'
get.edge.attribute.networkLite <- function(x, attrname, ..., null.na = TRUE, unlist = TRUE) {
  if (attrname %in% list.edge.attributes(x)) {
    out <- x$el[[attrname]]
  } else {
    out <- vector(mode = "list", length = network.edgecount(x, na.omit = FALSE))
  }

  if (null.na == TRUE && is.list(out)) {
    out <- lapply(out, NVL, NA)
  }

  if (unlist == TRUE) {
    out <- unlist(out)
  } else {
    out <- as.list(out)
  }

  return(out)
}

#' @rdname attribute_methods
#' @export
#'
get.edge.value.networkLite <- get.edge.attribute.networkLite

#' @rdname attribute_methods
#' @export
#'
set.edge.attribute.networkLite <- function(
    x, attrname, value,
    e = seq_len(network.edgecount(x, na.omit = FALSE)), ...) {

  xn <- substitute(x)

  if (!(attrname %in% list.edge.attributes(x))) {
    x$el[[attrname]] <- vector(mode = "list", length = network.edgecount(x, na.omit = FALSE))
  }

  x$el[[attrname]][e] <- value

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}

#' @rdname attribute_methods
#' @export
#'
set.edge.value.networkLite <- function(
    x, attrname, value,
    e = seq_len(network.edgecount(x, na.omit = FALSE)), ...) {

  xn <- substitute(x)

  if (!(attrname %in% list.edge.attributes(x))) {
    x$el[[attrname]] <- vector(mode = "list", length = network.edgecount(x, na.omit = FALSE))
  }

  x$el[[attrname]][e] <- value[as.matrix(x$el[e, c(".tail", ".head")])]

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}

#' @rdname attribute_methods
#' @export
#'
list.edge.attributes.networkLite <- function(x, ...) {
  sort(unique(colnames(x$el)[-c(1, 2)]))
}

#' @rdname attribute_methods
#' @export
delete.vertex.attribute.networkLite <- function(x, attrname, ...) {
  xn <- substitute(x)

  x$attr[[attrname]] <- NULL

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}

#' @rdname attribute_methods
#' @export
delete.edge.attribute.networkLite <- function(x, attrname, ...) {
  xn <- substitute(x)

  x$el[[attrname]] <- NULL

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}

#' @rdname attribute_methods
#' @export
delete.network.attribute.networkLite <- function(x, attrname, ...) {
  xn <- substitute(x)

  x$gal[[attrname]] <- NULL

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}
