
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
#'        attribute setters. For \code{set.vertex.attribute} and
#'        \code{set.edge.attribute}, \code{value} should be either an atomic
#'        vector or a list, of length equal to that of \code{v} or \code{e}.
#'        For \code{set.edge.value}, it should be an \code{n} by \code{n}
#'        matrix where \code{n} is the network size of \code{x}.
#' @param v Indices at which to set vertex attribute values.
#' @param e Indices at which to set edge attribute values.
#' @param null.na Logical. If \code{TRUE}, replace \code{NULL} attribute values
#'        with \code{NA} in \code{get.vertex.attribute} and
#'        \code{get.edge.attribute}. Applied before the \code{unlist} argument.
#'        Note that the behavior of \code{null.na} in \code{network} is
#'        somewhat different.
#' @param unlist Logical. In \code{get.vertex.attribute} and
#'        \code{get.edge.attribute}, if \code{unlist} is \code{TRUE}, we call
#'        \code{unlist} on the attribute value before returning it, and if
#'        \code{unlist} is \code{FALSE}, we call \code{as.list} on the
#'        attribute value before returning it. In \code{get.network.attribute},
#'        if \code{unlist} is \code{TRUE}, we call \code{unlist} on the
#'        attribute value before returning it, and if \code{unlist} is
#'        \code{FALSE}, we return the attribute value without any modification.
#' @param upcast Logical. Are we allowed to upcast atomic types when setting
#'        vertex or edge attribute values on the \code{networkLite}? Setting
#'        \code{upcast = FALSE} prevents upcasting, while setting
#'        \code{upcast = TRUE} allows but does not guarantee upcasting.
#' @param ... additional arguments
#'
#' @details Allows basic attribute manipulation for \code{networkLite}s. Note
#'          that an edge or vertex attribute not present in the
#'          \code{networkLite} is treated as a list of \code{NULL}s of length
#'          equal to the number of edges or vertices (respectively) before
#'          applying the \code{null.na} and \code{unlist} arguments.
#'
#' @return Behavior and return values are analogous to those of the
#'         corresponding \code{network} methods, with network data structured
#'         in the \code{networkLite} format.
#'
#' @export
#'
get.vertex.attribute.networkLite <- function(x, attrname, ..., null.na = TRUE, unlist = TRUE) {
  if (!(attrname %in% list.vertex.attributes(x))) {
    ## special case handling relevant to netsim efficiency
    if (null.na == TRUE && unlist == TRUE) {
      return(rep(NA, length.out = network.size(x)))
    } else if (null.na == TRUE && unlist == FALSE) {
      return(as.list(rep(NA, length.out = network.size(x))))
    } else if (null.na == FALSE && unlist == TRUE) {
      return(NULL)
    } else {
      return(vector(mode = "list", length = network.size(x)))
    }
  }

  out <- x$attr[[attrname]]

  if (null.na == TRUE && is.list(out)) {
    out <- lapply(out, function(val) if (!is.null(val)) val else NA)
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
                                             ...,
                                             upcast = FALSE) {
  xn <- substitute(x)

  if (missing(v)) {
    ## just set everything
    x$attr[[attrname]] <- rep(value, length.out = network.size(x))
  } else {
    if (!(attrname %in% list.vertex.attributes(x))) {
      ## new attr; set up as list since v isn't missing
      x$attr[[attrname]] <- vector(mode = "list", length = network.size(x))
    } else if (upcast == FALSE && !identical(class(value), class(x$attr[[attrname]]))) {
      ## existing attr; need to watch upcasting
      x$attr[[attrname]] <- as.list(x$attr[[attrname]])
    }
    x$attr[[attrname]][v] <- value
  }

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}

#' @rdname attribute_methods
#' @export
#'
list.vertex.attributes.networkLite <- function(x, ...) {
  if (network.size(x) == 0) {
    ## as in network...
    return(NULL)
  } else {
    return(sort(unique(names(x$attr))))
  }
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
get.edge.attribute.networkLite <- function(x, attrname, ..., null.na = FALSE, unlist = TRUE) {
  if (!(attrname %in% list.edge.attributes(x))) {
    ## special case handling consistent as for vertex attributes
    if (null.na == TRUE && unlist == TRUE) {
      return(rep(NA, length.out = network.edgecount(x, na.omit = FALSE)))
    } else if (null.na == TRUE && unlist == FALSE) {
      return(as.list(rep(NA, length.out = network.edgecount(x, na.omit = FALSE))))
    } else if (null.na == FALSE && unlist == TRUE) {
      return(NULL)
    } else {
      return(vector(mode = "list", length = network.edgecount(x, na.omit = FALSE)))
    }
  }

  out <- x$el[[attrname]]

  if (null.na == TRUE && is.list(out)) {
    out <- lapply(out, function(val) if (!is.null(val)) val else NA)
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
    e = seq_len(network.edgecount(x, na.omit = FALSE)), ..., upcast = FALSE) {

  xn <- substitute(x)

  if (missing(e)) {
    ## just set everything
    x$el[[attrname]] <- rep(value, length.out = network.edgecount(x, na.omit = FALSE))
  } else {
    if (!(attrname %in% list.edge.attributes(x))) {
      ## new attr; set up as list since e isn't missing
      x$el[[attrname]] <- vector(mode = "list", length = network.edgecount(x, na.omit = FALSE))
    } else if (upcast == FALSE && !identical(class(value), class(x$el[[attrname]]))) {
      ## existing attr; need to watch upcasting
      x$el[[attrname]] <- as.list(x$el[[attrname]])
    }
    x$el[[attrname]][e] <- value
  }

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}

#' @rdname attribute_methods
#' @export
#'
set.edge.value.networkLite <- function(
    x, attrname, value,
    e = seq_len(network.edgecount(x, na.omit = FALSE)), ..., upcast = FALSE) {

  xn <- substitute(x)

  value <- value[cbind(x$el$.tail[e], x$el$.head[e])]

  if (missing(e)) {
    ## just set everything
    x$el[[attrname]] <- rep(value, length.out = network.edgecount(x, na.omit = FALSE))
  } else {
    if (!(attrname %in% list.edge.attributes(x))) {
      ## new attr; set up as list since e isn't missing
      x$el[[attrname]] <- vector(mode = "list", length = network.edgecount(x, na.omit = FALSE))
    } else if (upcast == FALSE && !identical(class(value), class(x$el[[attrname]]))) {
      ## existing attr; need to watch upcasting
      x$el[[attrname]] <- as.list(x$el[[attrname]])
    }
    x$el[[attrname]][e] <- value
  }

  on.exit(eval.parent(call("<-", xn, x)))
  invisible(x)
}

#' @rdname attribute_methods
#' @export
#'
list.edge.attributes.networkLite <- function(x, ...) {
  if (network.edgecount(x, na.omit = FALSE) == 0) {
    ## as in network...
    return(character(0))
  } else {
    return(sort(unique(setdiff(names(x$el), c(".tail", ".head")))))
  }
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
