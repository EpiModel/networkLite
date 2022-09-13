## atomize a networkLite
## convert vertex and edge attributes to atomic vectors where possible;
## note that this may upcast atomic types, e.g. logical -> numeric -> character
atomize <- function(nwL) {
  nwL$el <- atomize_tibble(nwL$el) # also applies to .tail, .head
  nwL$attr <- atomize_tibble(nwL$attr)
  nwL
}

## atomize a tibble; as for networkLites
atomize_tibble <- function(x) {
  for (name in names(x)) {
    value <- x[[name]]
    if (length(value) > 0 &&
        all(unlist(lapply(value, is.atomic))) &&
        all(unlist(lapply(value, length)) == 1)) {
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
