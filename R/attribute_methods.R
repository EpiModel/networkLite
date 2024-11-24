
#' @rdname attribute_methods
#'
#' @title `networkLite` Attribute Methods
#'
#' @description S3 attribute methods for the `networkLite` class, for
#'              generics defined in the `network` package.
#'
#' @param x A `networkLite` object.
#' @param attrname The name of an attribute in `x`; must be a length one
#'        character vector.
#' @param value The attribute value to set in vertex, edge, and network
#'        attribute setters. For `set.vertex.attribute` and
#'        `set.edge.attribute`, `value` should be either an atomic
#'        vector or a list, of length equal to that of `v` or `e`.
#'        For `set.edge.value`, it should be an `n` by `n`
#'        matrix where `n` is the network size of `x`.
#' @param v Indices at which to set vertex attribute values.
#' @param e Indices at which to set edge attribute values.
#' @param null.na Logical. If `TRUE`, replace `NULL` attribute values
#'        with `NA` in `get.vertex.attribute` and
#'        `get.edge.attribute`. Applied before the `unlist` argument.
#'        Note that the behavior of `null.na` in `network` is
#'        somewhat different.
#' @param unlist Logical. In `get.vertex.attribute` and
#'        `get.edge.attribute`, if `unlist` is `TRUE`, we call
#'        `unlist` on the attribute value before returning it, and if
#'        `unlist` is `FALSE`, we call `as.list` on the
#'        attribute value before returning it. In `get.network.attribute`,
#'        if `unlist` is `TRUE`, we call `unlist` on the
#'        attribute value before returning it, and if `unlist` is
#'        `FALSE`, we return the attribute value without any modification.
#' @param upcast Logical. Are we allowed to upcast atomic types when setting
#'        vertex or edge attribute values on the `networkLite`? Setting
#'        `upcast = FALSE` prevents upcasting, while setting
#'        `upcast = TRUE` allows but does not guarantee upcasting.
#' @param ... additional arguments
#'
#' @details Allows basic attribute manipulation for `networkLite`s. Note
#' that an edge or vertex attribute not present in the
#' `networkLite` is treated as a list of `NULL`s of length
#' equal to the number of edges or vertices (respectively) before
#' applying the `null.na` and `unlist` arguments.
#'
#' @return Behavior and return values are analogous to those of the
#' corresponding `network` methods, with network data structured
#' in the `networkLite` format.
#'
#' @export
#'
get.vertex.attribute.networkLite <- function(x, attrname, ..., null.na = TRUE, unlist = TRUE) {
  if (is.null(out <- unclass(x$attr)[[attrname]])) {
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

  modify_in_place(x)
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
  x$gal[[attrname]] <- value

  modify_in_place(x)
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
  if (is.null(out <- unclass(x$el)[[attrname]])) {
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

  modify_in_place(x)
}

#' @rdname attribute_methods
#' @export
#'
set.edge.value.networkLite <- function(
    x, attrname, value,
    e = seq_len(network.edgecount(x, na.omit = FALSE)), ..., upcast = FALSE) {

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

  modify_in_place(x)
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
  ## TODO: See if this can be done in one operation faster.
  for(a in attrname) x$attr[[a]] <- NULL

  modify_in_place(x)
}

#' @rdname attribute_methods
#' @export
delete.edge.attribute.networkLite <- function(x, attrname, ...) {
  ## TODO: See if this can be done in one operation faster.
  for(a in attrname) x$el[[a]] <- NULL

  modify_in_place(x)
}

#' @rdname attribute_methods
#' @export
delete.network.attribute.networkLite <- function(x, attrname, ...) {
  ## TODO: See if this can be done in one operation faster.
  for(a in attrname) x$gal[[a]] <- NULL

  modify_in_place(x)
}
