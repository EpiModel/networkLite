
#' networkLite Package
#'
#' \tabular{ll}{
#'    Package: \tab networkLite\cr
#'    Type: \tab Package\cr
#'    Version: \tab 1.0.0\cr
#'    Date: \tab 2023-01-17\cr
#'    License: \tab GPL-3\cr
#'    LazyLoad: \tab yes\cr
#' }
#'
#' @details
#' The `networkLite` package provides an alternative implementation of
#' some of the functionality in the `network` package, based on a
#' different data structure that is faster for certain applications. It is
#' intended for use as a backend data structure in `EpiModel` and
#' `statnet` packages, and its implementation is subject to change.
#'
#' The `networkLite` data structure is a named list with three components:
#' * `el`, a `tibble` edgelist, including edge attributes
#' * `attr`, a `tibble` of vertex attributes
#' * `gal`, a named list of network attributes
#'
#' These components should not be referred to directly by the user in their own
#' code. Instead, the various access, coercion, etc. methods provided by this
#' package should be used. See [`networkLite`] for information on
#' how to construct a `networkLite`.
#'
#' Certain names in `el`, `attr`, and `gal` have special significance. These are:
#' * For `el`: `".tail"` and `".head"`, of class integer, which are the tails and
#'   heads of edges, and must be preserved as atomic integer vectors with no `NA`s;
#'   `"na"`, which is a logical attribute indicating if the edge is missing or not,
#'   and should take `TRUE`/`FALSE` values only (behavior for other values is
#'   undefined, and `NA`s are not allowed); `"na"` may be structured as either an
#'   atomic logical vector or a list.
#' * For `attr`: `"na"`, which is a logical attribute indicating if the vertex is
#'   missing or not, and `"vertex.names"`, which provides names for the vertices in
#'   the network; the attribute `"na"` should take values `TRUE` or `FALSE` only
#'   (behavior for other values is undefined).
#' * For `gal`: `"n"` (the network size), `"directed"` (a logical indicating if the
#'   network is directed), `"bipartite"` (either `FALSE` to indicate the network is
#'   not bipartite, or the size of the first bipartition if the network is bipartite),
#'   `"hyper"` (a logical indicating if the network is a hypergraph), `"multiple"`
#'   (a logical indicating if the network is a multigraph), and `"loops"` (a logical
#'   indicating if the network is allowed to have loops).
#'
#' For `networkLite`s, the three network attributes `"hyper"`,
#' `"multiple"`, and `"loops"` must all be `FALSE`. Even with
#' these restrictions, `networkLite`s do not provide all the functionality
#' that `network`s do, but attempt to offer what is necessary for backend
#' use in `ergm`, `tergm`, and `EpiModel`.
#'
#' @name networkLite-package
#'
#' @import network
#' @importFrom statnet.common NVL NVL2
#' @importFrom tibble tibble as_tibble is_tibble
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom stats na.omit
#'
NULL
