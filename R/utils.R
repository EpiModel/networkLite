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

## test two networks or two networkLites for equivalent attributes, edges, etc.
expect_equiv_nets <- function(nw1, nw2) {
  expect_identical(list.network.attributes(nw1),
                   list.network.attributes(nw2))

  expect_identical(list.vertex.attributes(nw1),
                   list.vertex.attributes(nw2))

  expect_identical(list.edge.attributes(nw1),
                   list.edge.attributes(nw2))

  for (attrname in list.network.attributes(nw1)) {
    if (attrname == "n") {
      # can have integer vs. double issues...
      expect_equal(get.network.attribute(nw1, attrname),
                   get.network.attribute(nw2, attrname))    
    } else {
      expect_identical(get.network.attribute(nw1, attrname),
                       get.network.attribute(nw2, attrname))
    }
  }

  for (attrname in list.vertex.attributes(nw1)) {
    expect_identical(get.vertex.attribute(nw1, attrname, null.na = FALSE, unlist = FALSE),
                     get.vertex.attribute(nw2, attrname, null.na = FALSE, unlist = FALSE))
  }

  expect_identical(as.edgelist(nw1, na.rm = FALSE), as.edgelist(nw2, na.rm = FALSE))
  el <- as.edgelist(nw1, na.rm = FALSE)
  if (!is(nw1, "networkLite")) {
    eids1 <- unlist(lapply(seq_len(NROW(el)), function(index) get.edgeIDs(nw1, el[index, 1], el[index, 2], na.omit = FALSE)))
  } else {
    eids1 <- seq_len(network.edgecount(nw1, na.omit = FALSE))
  }
  if (!is(nw2, "networkLite")) {
    eids2 <- unlist(lapply(seq_len(NROW(el)), function(index) get.edgeIDs(nw2, el[index, 1], el[index, 2], na.omit = FALSE)))
  } else {
    eids2 <- seq_len(network.edgecount(nw2, na.omit = FALSE))  
  }
  for (attrname in list.edge.attributes(nw1)) {
    expect_identical(get.edge.attribute(nw1, attrname, null.na = FALSE, unlist = FALSE, na.omit = FALSE, deleted.edges.omit = FALSE)[eids1],
                     get.edge.attribute(nw2, attrname, null.na = FALSE, unlist = FALSE, na.omit = FALSE, deleted.edges.omit = FALSE)[eids2])
  }

  sn1 <- sort(names(attributes(nw1)))
  sn2 <- sort(names(attributes(nw2)))
  expect_identical(sn1, sn2)
  expect_identical(length(sn1), length(attributes(nw1)))
  expect_identical(attributes(nw1)[sn1], attributes(nw2)[sn2])
}

## create a random edgelist with the specified nodes, directed, bipartite properties,
## and (on average) target_n_edges number of edges; used in tests to avoid san calls,
## so that networkLite does not rely on ergm
create_random_edgelist <- function(n_nodes, directed, bipartite, target_n_edges) {
  if (directed == TRUE) {
    ## directed unipartite
    adj <- matrix(rbinom(n_nodes*n_nodes, 1, target_n_edges/(n_nodes*(n_nodes - 1))), nrow = n_nodes, ncol = n_nodes)
    diag(adj) <- 0
  } else if (bipartite > 0) {
    ## undirected bipartite
    bip <- matrix(rbinom(bipartite*(n_nodes - bipartite), 1, target_n_edges/(bipartite*(n_nodes - bipartite))), nrow = bipartite, ncol = n_nodes - bipartite)
    adj <- matrix(0, nrow = n_nodes, ncol = n_nodes)
    adj[seq_len(bipartite), -seq_len(bipartite)] <- bip
  } else {
    ## undirected unipartite
    adj <- matrix(rbinom(n_nodes*n_nodes, 1, 2*target_n_edges/(n_nodes*(n_nodes - 1))), nrow = n_nodes, ncol = n_nodes)
    adj[lower.tri(adj, diag = TRUE)] <- 0
  }
  el <- which(adj > 0, arr.ind = TRUE)
  el <- el[order(el[,1], el[,2]),,drop=FALSE]
  colnames(el) <- c(".tail", ".head")
  structure(el, n = n_nodes, directed = directed, bipartite = bipartite)
}
