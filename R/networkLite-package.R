
#' @title networkLite Package
#' @description
#' The \code{networkLite} package provides an alternative implementation of
#' some of the functionality in the \code{network} package, based on a
#' different data structure that is faster for certain applications. It is
#' intended for use as a backend data structure in \code{EpiModel} and
#' \code{statnet} packages, and its implementation is subject to change without
#' notice.
#'
#' The \code{networkLite} data structure is a named list with three components:
#' \itemize{
#'  \item \code{el}, a \code{tibble} edgelist, including edge attributes
#'  \item \code{attr}, a \code{tibble} of vertex attributes
#'  \item \code{gal}, a named list of network attributes
#' }
#' These components should not be referred to directly by the user in their own
#' code. Instead, the various access, coercion, etc. methods provided by this
#' package should be used. See \code{\link{constructors}} for information on
#' how to construct a \code{networkLite}.
#'
#' Certain names in \code{el}, \code{attr}, and \code{gal} have special
#' significance. These are
#' \itemize{
#'  \item for \code{el}: \code{".tail"} and \code{".head"}, which are the tails
#'  and heads of edges, and \code{"na"}, which is a logical attribute indicating
#'  if the edge is missing or not;
#'  \item for \code{attr}: \code{"na"}, which is a logical attribute indicating
#'  if the vertex is missing or not;
#'  \item for \code{gal}: \code{"n"} (the network size), \code{"directed"} (a
#'  logical indicating if the network is directed), \code{"bipartite"} (either
#'  \code{FALSE} to indicate the network is not bipartite, or the size of the
#'  first bipartition if the network is bipartite), \code{"hyper"} (a logical
#'  indicating if the network is a hypergraph), \code{"multiple"} (a logical
#'  indicating if the network is a multigraph), and \code{"loops"} (a logical
#'  indicating if the network is allowed to have loops).
#' }
#' For \code{networkLite}s, the three network attributes \code{"hyper"},
#' \code{"multiple"}, and \code{"loops"} must all be \code{FALSE}. Even with
#' these restrictions, \code{networkLite}s do not provide all the functionality
#' that \code{network}s do, but attempt to offer what is necessary for backend
#' use in \code{ergm}, \code{tergm}, and \code{EpiModel}.
#'
#' @name networkLite-package
#' @aliases networkLite
#'
#' @import network
#' @importFrom statnet.common NVL NVL2
#' @importFrom tibble tibble as_tibble is_tibble
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom stats na.omit
#'
NULL
