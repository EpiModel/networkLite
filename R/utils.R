#' @rdname atomize
#' @title Convert Lists to Atomic Vectors Where Possible
#' @param x A \code{networkLite} or \code{tibble} object.
#' @param upcast logical; are we allowed to upcast atomic types when converting
#'        lists to atomic vectors?
#' @param ... additional arguments
#' @return The \code{networkLite} or \code{tibble} with list columns replaced by
#'         atomic vector columns where possible.
#' @details The \code{tibble} method examines each column of the \code{tibble}
#'          and replaces the column with the result of calling \code{unlist} on
#'          the column if all of the following are true: the column
#'          \code{is.list} of length greater than zero, each element of which
#'          \code{is.atomic} of length one, and either \code{upcast} is
#'          \code{TRUE} or there is only one unique class amongst all elements
#'          of the column.
#'
#'          The \code{networkLite} method applies the \code{tibble} method to
#'          the edgelist and vertex attribute \code{tibble}s in the
#'          \code{networkLite}.
#'
#' @export
#'
atomize <- function(x, ...) {
  UseMethod("atomize")
}

#' @rdname atomize
#' @export
#'
atomize.networkLite <- function(x, ..., upcast = FALSE) {
  x$el <- atomize(x$el, ..., upcast = upcast) # also applies to .tail, .head
  x$attr <- atomize(x$attr, ..., upcast = upcast)
  x
}

#' @rdname atomize
#' @export
#'
atomize.tbl_df <- function(x, ..., upcast = FALSE) {
  for (name in names(x)) {
    value <- x[[name]]
    if (is.list(value) &&
        length(value) > 0 &&
        all(unlist(lapply(value, is.atomic))) &&
        all(unlist(lapply(value, length)) == 1) &&
        (upcast == TRUE || length(unique(unlist(lapply(value, class)))) == 1)) {
      x[[name]] <- unlist(value)
    }
  }
  x
}

## x = a list of tibbles; this function ensures that if any attribute is stored
## as a list in any tibble in x, then it is stored as a list in all tibbles in x;
## needed to avoid errors in dplyr::bind_rows
ensure_list <- function(x) {
  names <- sort(unique(unlist(lapply(x, names))))
  for (name in names) {
    any_list <- any(unlist(lapply(lapply(x, `[[`, name), is.list)))
    if (any_list == TRUE) {
      x <- lapply(x, function(y) {
                       if (name %in% names(y)) {
                         y[[name]] <- as.list(y[[name]])
                       }
                       y
                     })
    }
  }
  return(x)
}
