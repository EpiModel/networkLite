## test two networks or networkLites for equivalent attributes, edges, etc.
## note that atomic type upcasting in as.networkLite can create comparison issues
expect_equiv_nets <- function(nw1, nw2, skip.mnext = FALSE) {
  if ((is(nw1, "networkLite") && is(nw2, "networkLite")) ||
      (!is(nw1, "networkLite") && !is(nw2, "networkLite"))) {
    net_attr_1 <- list.network.attributes(nw1)
    net_attr_2 <- list.network.attributes(nw2)
  } else {
    if (is(nw1, "networkLite")) {
      net_attr_1 <- list.network.attributes(nw1)
      net_attr_2 <- setdiff(list.network.attributes(nw2), "mnext")
    } else {
      net_attr_1 <- setdiff(list.network.attributes(nw1), "mnext")      
      net_attr_2 <- list.network.attributes(nw2)
    }
  }

  if (skip.mnext == TRUE) {
    net_attr_1 <- setdiff(net_attr_1, "mnext")
    net_attr_2 <- setdiff(net_attr_2, "mnext")
  }

  expect_identical(net_attr_1,
                   net_attr_2)

  expect_identical(list.vertex.attributes(nw1),
                   list.vertex.attributes(nw2))

  expect_identical(list.edge.attributes(nw1),
                   list.edge.attributes(nw2))

  for (attrname in net_attr_1) {
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

  expect_equal(as.edgelist(nw1, na.rm = FALSE), as.edgelist(nw2, na.rm = FALSE))
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
  sn1 <- setdiff(sn1, c("names", "class"))
  sn2 <- setdiff(sn2, c("names", "class"))
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

test_that("%e%<- behaves as expected", {
  net_size <- 100
  bip_size <- 40
  edges_target <- net_size

  for(directed in list(FALSE, TRUE)) {
    for(bipartite in list(FALSE, bip_size)) {
      if(directed && bipartite) {
        next
      }

      set.seed(0)
      nw <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")
      vals <- runif(network.edgecount(nw))
      valmat <- matrix(runif(network.size(nw)*network.size(nw)), nrow = network.size(nw))
      nw %e% "a1" <- vals
      nw %e% "a2" <- valmat

      set.seed(0)
      nwL <- networkLite(create_random_edgelist(net_size, directed, bipartite, edges_target))
      vals <- runif(network.edgecount(nwL))
      valmat <- matrix(runif(network.size(nwL)*network.size(nwL)), nrow = network.size(nwL))
      nwL %e% "a1" <- vals
      nwL %e% "a2" <- valmat

      expect_equiv_nets(nw, nwL)
    }
  }
})

test_that("add edges, add vertices, and ensure_list", {
  el <- cbind(1:3, 2:4)
  attr(el, "n") <- 5
  nwL <- networkLite(el)
  set.edge.attribute(nwL, "e1", 1:3) # compatible atomics
  set.edge.attribute(nwL, "e2", 1:3 + 0.5) # incompatible atomics
  set.edge.attribute(nwL, "e3", letters[1:3]) # NULL atomic
  ## no e4 here # atomic NULL
  set.edge.attribute(nwL, "e5", list("a","b","c")) # NULL list
  ## no e6 here # list NULL
  set.edge.attribute(nwL, "e7", list("a","b","c")) # atomic list
  set.edge.attribute(nwL, "e8", c(FALSE, FALSE, TRUE)) # list atomic

  tail <- c(1,3)
  head <- c(4,5)
  names <- c("e1", "e2", "e4", "e6", "e7", "e8")
  names <- rep(list(names), length.out = 2)
  vals <- list(list(1L, 1L, FALSE, list(1), "a", list("a")),
               list(2L, 2L, TRUE, list(2), "b", list("b")))
  
  nwL <- add.edges(nwL, tail = tail, head = head, names.eval = names, vals.eval = vals)
  expect_identical(nwL$el$na, logical(5))
  nwL <- add.edges(nwL, tail = c(1,1), head = c(3,5), names.eval = list("na", "na"), vals.eval = list(list(FALSE),list(TRUE)))
  expect_identical(nwL$el$na, as.list(c(rep(FALSE, 3), TRUE, rep(FALSE, 3))))
  nwL <- atomize(nwL, upcast = FALSE)
  nwL <- atomize(nwL, upcast = TRUE)
})

test_that("setting vertex and edge attributes", {
  nw <- network.initialize(5, directed = FALSE)

  nw[1,2] <- 1
  nw[1,3] <- 1
  nw[2,4] <- 1
  nw[3,5] <- 1

  set.vertex.attribute(nw, "v1", list(1,2L))
  set.vertex.attribute(nw, "v2", list(1,2L,"a"))
  set.vertex.attribute(nw, "v3", c(1,2))
  set.edge.attribute(nw, "e1", list(1,2L))
  set.edge.attribute(nw, "e2", list(1,2L,"a"))
  set.edge.attribute(nw, "e3", c(1,2))

  nwL <- as.networkLite(nw)
  set.vertex.attribute(nwL, "v1", list(1,2L))
  set.vertex.attribute(nwL, "v2", list(1,2L,"a"))
  set.vertex.attribute(nwL, "v3", c(1,2))
  set.edge.attribute(nwL, "e1", list(1,2L))
  set.edge.attribute(nwL, "e2", list(1,2L,"a"))
  set.edge.attribute(nwL, "e3", c(1,2))

  expect_equiv_nets(nw, nwL)

  expect_identical(nwL$el[["e3"]], c(1,2,1,2))

  set.edge.attribute(nwL, "e3", c(1L, 2L), c(1,3), upcast = FALSE)
  expect_identical(nwL$el[["e3"]], list(1L,2,2L,2))
  set.edge.attribute(nwL, "e3", c(1,2))
  expect_identical(nwL$el[["e3"]], c(1,2,1,2))
  nwL <- atomize(nwL)
  expect_identical(nwL$el[["e3"]], c(1,2,1,2))
  set.edge.attribute(nwL, "e3", c(1L, 2L), c(1,3), upcast = TRUE)
  expect_identical(nwL$el[["e3"]], c(1,2,2,2))
  delete.edge.attribute(nwL, "e3")
  set.edge.attribute(nwL, "e3", c(1L,2L,3L,4L))
  expect_identical(nwL$el[["e3"]], c(1L,2L,3L,4L))
  set.edge.attribute(nwL, "e3", c(1, 2), c(1,3), upcast = FALSE)
  expect_identical(nwL$el[["e3"]], list(1,2L,2,4L))
  set.edge.attribute(nwL, "e3", c(1L,2L,3L,4L))
  expect_identical(nwL$el[["e3"]], c(1L,2L,3L,4L))
  set.edge.attribute(nwL, "e3", c(1, 2), c(1,3), upcast = TRUE)
  expect_identical(nwL$el[["e3"]], c(1,2,2,4))
  set.edge.attribute(nwL, "e3", list(1, 2))
  expect_identical(nwL$el[["e3"]], list(1,2,1,2))

  expect_identical(nwL$attr[["v3"]], c(1,2,1,2,1))

  set.vertex.attribute(nwL, "v3", c(1L, 2L), c(1,3), upcast = FALSE)
  expect_identical(nwL$attr[["v3"]], list(1L,2,2L,2,1))
  set.vertex.attribute(nwL, "v3", c(1,2))
  expect_identical(nwL$attr[["v3"]], c(1,2,1,2,1))
  nwL <- atomize(nwL)
  expect_identical(nwL$attr[["v3"]], c(1,2,1,2,1))
  set.vertex.attribute(nwL, "v3", c(1L, 2L), c(1,3), upcast = TRUE)
  expect_identical(nwL$attr[["v3"]], c(1,2,2,2,1))
  delete.vertex.attribute(nwL, "v3")
  set.vertex.attribute(nwL, "v3", c(1L,2L,3L,4L,5L))
  expect_identical(nwL$attr[["v3"]], c(1L,2L,3L,4L,5L))
  set.vertex.attribute(nwL, "v3", c(1, 2), c(1,3), upcast = FALSE)
  expect_identical(nwL$attr[["v3"]], list(1,2L,2,4L,5L))
  set.vertex.attribute(nwL, "v3", c(1L,2L,3L,4L,5L))
  expect_identical(nwL$attr[["v3"]], c(1L,2L,3L,4L,5L))
  set.vertex.attribute(nwL, "v3", c(1, 2), c(1,3), upcast = TRUE)
  expect_identical(nwL$attr[["v3"]], c(1,2,2,4,5))
  set.vertex.attribute(nwL, "v3", list(1, 2))
  expect_identical(nwL$attr[["v3"]], list(1,2,1,2,1))
})

test_that("atomize and upcast work as intended", {
  nw <- network.initialize(5, directed = FALSE)

  nw[1,2] <- 1
  nw[1,3] <- 1
  nw[2,4] <- 1
  nw[3,5] <- 1

  set.vertex.attribute(nw, "v1", list(1,2,"a","b",FALSE))
  set.vertex.attribute(nw, "v2", list(1,2,NULL,4,5))
  set.vertex.attribute(nw, "v3", list(1,2,3L,4,5))
  set.vertex.attribute(nw, "v4", list(1,2,3,4,5))
  set.edge.attribute(nw, "e1", list(1,2,"a",FALSE))
  set.edge.attribute(nw, "e2", list(1,2,NULL,4))
  set.edge.attribute(nw, "e3", list(1,2,3L,4))
  set.edge.attribute(nw, "e4", list(1,2,3,4))

  nwL <- as.networkLite(nw)
  nwL_na <- as.networkLite(nw, atomize = FALSE)
  nwL_nu <- as.networkLite(nw, upcast = FALSE)
  
  expect_identical(nwL$el[["e1"]], c("1","2","a","FALSE"))
  expect_identical(nwL$el[["e2"]], list(1,2,NULL,4))
  expect_identical(nwL$el[["e3"]], c(1,2,3,4))
  expect_identical(nwL$el[["e4"]], c(1,2,3,4))

  expect_identical(nwL$attr[["v1"]], c("1","2","a","b","FALSE"))
  expect_identical(nwL$attr[["v2"]], list(1,2,NULL,4,5))
  expect_identical(nwL$attr[["v3"]], c(1,2,3,4,5))
  expect_identical(nwL$attr[["v4"]], c(1,2,3,4,5))
  
  expect_identical(nwL_na$el[["e1"]], list(1,2,"a",FALSE))
  expect_identical(nwL_na$el[["e2"]], list(1,2,NULL,4))
  expect_identical(nwL_na$el[["e3"]], list(1,2,3L,4))
  expect_identical(nwL_na$el[["e4"]], list(1,2,3,4))

  expect_identical(nwL_na$attr[["v1"]], list(1,2,"a","b",FALSE))
  expect_identical(nwL_na$attr[["v2"]], list(1,2,NULL,4,5))
  expect_identical(nwL_na$attr[["v3"]], list(1,2,3L,4,5))
  expect_identical(nwL_na$attr[["v4"]], list(1,2,3,4,5))

  expect_identical(nwL_nu$el[["e1"]], list(1,2,"a",FALSE))
  expect_identical(nwL_nu$el[["e2"]], list(1,2,NULL,4))
  expect_identical(nwL_nu$el[["e3"]], list(1,2,3L,4))
  expect_identical(nwL_nu$el[["e4"]], c(1,2,3,4))

  expect_identical(nwL_nu$attr[["v1"]], list(1,2,"a","b",FALSE))
  expect_identical(nwL_nu$attr[["v2"]], list(1,2,NULL,4,5))
  expect_identical(nwL_nu$attr[["v3"]], list(1,2,3L,4,5))
  expect_identical(nwL_nu$attr[["v4"]], c(1,2,3,4,5))
})

test_that("setting vertex and edge attributes in strange ways", {
  el <- cbind(c(1,2,3,3), c(2,5,4,5))
  attr(el, "n") <- 5
  nw <- network(el, directed = FALSE, bipartite = FALSE, matrix.type = "edgelist")
  set.edge.attribute(nw, "ae1", 1:4)
  set.edge.attribute(nw, "ae2", 1:4)
  set.vertex.attribute(nw, "av1", letters[1:5])
  set.vertex.attribute(nw, "av2", letters[1:5])

  set.edge.attribute(nw, "ae1", 1:2, c(1,2,4))
  set.edge.attribute(nw, "ae2", 1:3, c(3,1,4,2))
  set.vertex.attribute(nw, "av1", letters[1:7])
  set.vertex.attribute(nw, "av2", c("1","2","3"), 1:2)

  nwL <- networkLite(el)
  set.edge.attribute(nwL, "ae1", 1:4)
  set.edge.attribute(nwL, "ae2", 1:4)
  set.vertex.attribute(nwL, "av1", letters[1:5])
  set.vertex.attribute(nwL, "av2", letters[1:5])
  nwL <- atomize(nwL)

  set.edge.attribute(nwL, "ae1", 1:2, c(1,2,4))
  set.edge.attribute(nwL, "ae2", 1:3, c(3,1,4,2))
  set.vertex.attribute(nwL, "av1", letters[1:7])
  set.vertex.attribute(nwL, "av2", c("1","2","3"), 1:2)
  
  expect_equiv_nets(nw, nwL)

  set.edge.attribute(nw, "ae1", list(list(1,2), list(3)), c(1,2,4))
  set.edge.attribute(nw, "ae2", list(list("a"), network.initialize(3), 1), c(3,1,4,2))
  set.vertex.attribute(nw, "av1", list(list(network.initialize(3)), 2, 3, "a", "b"))
  set.vertex.attribute(nw, "av2", list(list(network.initialize(3)), 2, 3, "a", "b"), 4:5)
  
  set.edge.attribute(nwL, "ae1", list(list(1,2), list(3)), c(1,2,4))
  set.edge.attribute(nwL, "ae2", list(list("a"), network.initialize(3), 1), c(3,1,4,2))
  set.vertex.attribute(nwL, "av1", list(list(network.initialize(3)), 2, 3, "a", "b", "c"))
  set.vertex.attribute(nwL, "av2", list(list(network.initialize(3)), 2, 3, "a", "b"), 4:5)

  expect_equiv_nets(nw, nwL)

})

test_that("accessing non-present attributes", {
  nwL <- networkLite(0)
  expect_identical(logical(0), get.vertex.attribute(nwL, "not_here", null.na = TRUE, unlist = TRUE))
  expect_identical(list(), get.vertex.attribute(nwL, "not_here", null.na = TRUE, unlist = FALSE))
  expect_identical(NULL, get.vertex.attribute(nwL, "not_here", null.na = FALSE, unlist = TRUE))
  expect_identical(list(), get.vertex.attribute(nwL, "not_here", null.na = FALSE, unlist = FALSE))

  expect_identical(logical(0), get.edge.attribute(nwL, "not_here", null.na = TRUE, unlist = TRUE))
  expect_identical(list(), get.edge.attribute(nwL, "not_here", null.na = TRUE, unlist = FALSE))
  expect_identical(NULL, get.edge.attribute(nwL, "not_here", null.na = FALSE, unlist = TRUE))
  expect_identical(list(), get.edge.attribute(nwL, "not_here", null.na = FALSE, unlist = FALSE))

  el <- cbind(c(1,1,2),c(2,3,4))
  attr(el, "n") <- 4
  nwL <- networkLite(el)
  expect_identical(c(NA,NA,NA,NA), get.vertex.attribute(nwL, "not_here", null.na = TRUE, unlist = TRUE))
  expect_identical(list(NA,NA,NA,NA), get.vertex.attribute(nwL, "not_here", null.na = TRUE, unlist = FALSE))
  expect_identical(NULL, get.vertex.attribute(nwL, "not_here", null.na = FALSE, unlist = TRUE))
  expect_identical(list(NULL,NULL,NULL,NULL), get.vertex.attribute(nwL, "not_here", null.na = FALSE, unlist = FALSE))

  expect_identical(c(NA,NA,NA), get.edge.attribute(nwL, "not_here", null.na = TRUE, unlist = TRUE))
  expect_identical(list(NA,NA,NA), get.edge.attribute(nwL, "not_here", null.na = TRUE, unlist = FALSE))
  expect_identical(NULL, get.edge.attribute(nwL, "not_here", null.na = FALSE, unlist = TRUE))
  expect_identical(list(NULL,NULL,NULL), get.edge.attribute(nwL, "not_here", null.na = FALSE, unlist = FALSE))  
})

test_that("various attribute operations function equivalently for network and networkLite", {
  ## vertex attributes
  nw <- network.initialize(5, directed = FALSE)  
  nwL <- networkLite(5)
  expect_equiv_nets(nw, nwL)

  net_list <- list(nw, nwL)
  
  for (i in seq_along(net_list)) {
    net <- net_list[[i]]

    set.vertex.attribute(net, "v1", 1:3, 1:3)
    set.vertex.attribute(net, "v2", as.list(2:4), 2:4)
    net %v% "v3" <- list(1:5)
    net %v% "v4" <- as.list(1:5)
    net %v% "v5" <- as.character(1:5)
    set.vertex.attribute(net, "v6", 1:3, 1:3)
    set.vertex.attribute(net, "v6", letters[3:5], 3:5)
    set.vertex.attribute(net, "v7", list(2:4), 2:4)
    set.vertex.attribute(net, "v7", list(1:3), c(1, 3, 5))

    net_list[[i]] <- net
  }
  
  nw <- net_list[[1]]
  nwL <- net_list[[2]]
  
  expect_is(nw, "network")
  expect_error(expect_is(nw, "networkLite"))
  expect_is(nwL, "network")
  expect_is(nwL, "networkLite")

  expect_equiv_nets(nw, nwL)

  ## edge attributes
  el <- cbind(c(1,1,3,3,4), c(2,3,4,5,5))
  attr(el, "n") <- 5
  nw <- network(el, directed = FALSE)
  nwL <- networkLite(el)
  expect_equiv_nets(nw, nwL)

  net_list <- list(nw, nwL)
  
  for (i in seq_along(net_list)) {
    net <- net_list[[i]]

    set.edge.attribute(net, "e1", 1:3, 1:3)
    set.edge.attribute(net, "e2", as.list(2:4), 2:4)
    set.edge.attribute(net, "e3", list(1:5))
    set.edge.attribute(net, "e4", as.list(1:5))
    set.edge.attribute(net, "e5", as.character(1:5))
    set.edge.attribute(net, "e6", 1:3, 1:3)
    set.edge.attribute(net, "e6", letters[3:5], 3:5)
    set.edge.attribute(net, "e7", list(2:4), 2:4)
    set.edge.attribute(net, "e7", list(1:3), c(1, 3, 5))

    net_list[[i]] <- net
  }
  
  nw <- net_list[[1]]
  nwL <- net_list[[2]]
  
  expect_is(nw, "network")
  expect_error(expect_is(nw, "networkLite"))
  expect_is(nwL, "network")
  expect_is(nwL, "networkLite")

  expect_equiv_nets(nw, nwL)
})

test_that("is.na, +, and - treat attributes as for network", {
  net_size <- 100
  bip_size <- 40
  edges_target <- net_size

  for(directed in list(FALSE, TRUE)) {
    for(bipartite in list(FALSE, bip_size)) {
      if(directed && bipartite) {
        next
      }

      for(last.mode in list(FALSE, TRUE)) {
        set.seed(0)
        nw0 <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")
        nw0 %v% "b" <- runif(net_size)
        nw0 %e% "eattr" <- runif(network.edgecount(nw0))
        nw0 %n% "nattr" <- "attr"
        add.vertices(nw0, 9, vattr = rep(list(list(na = FALSE, vertex.names = NA_integer_, b = NA_real_)), 9), last.mode = last.mode)

        set.seed(1)
        nw1 <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")
        nw1 %v% "b" <- runif(net_size)
        nw1 %e% "eattr" <- runif(network.edgecount(nw1))
        nw1 %n% "nattr" <- "attr"
        add.vertices(nw1, 9, vattr = rep(list(list(na = FALSE, vertex.names = NA_integer_, b = NA_real_)), 9), last.mode = last.mode)

        nw2 <- network.initialize(nw0 %n% "n", directed = nw0 %n% "directed", bipartite = nw0 %n% "bipartite")

        set.seed(0)
        nwL0 <- networkLite(create_random_edgelist(net_size, directed, bipartite, edges_target))
        nwL0 %v% "b" <- runif(net_size)
        set.edge.attribute(nwL0, "eattr", runif(network.edgecount(nwL0)))
        nwL0 %n% "nattr" <- "attr"
        add.vertices(nwL0, 9, vattr = rep(list(list(na = FALSE, vertex.names = NA_integer_, b = NA_real_)), 9), last.mode = last.mode)

        set.seed(1)
        nwL1 <- networkLite(create_random_edgelist(net_size, directed, bipartite, edges_target))
        nwL1 %v% "b" <- runif(net_size)
        set.edge.attribute(nwL1, "eattr", runif(network.edgecount(nwL1)))
        nwL1 %n% "nattr" <- "attr"
        add.vertices(nwL1, 9, vattr = rep(list(list(na = FALSE, vertex.names = NA_integer_, b = NA_real_)), 9), last.mode = last.mode)

        nwL2 <- networkLite(nwL0 %n% "n", directed = nwL0 %n% "directed", bipartite = nwL0 %n% "bipartite")

        expect_equiv_nets(nw0, nwL0)
        expect_equiv_nets(is.na(nw0), is.na(nwL0))
        expect_equiv_nets(is.na(is.na(nw0)), is.na(is.na(nwL0)))
        expect_equiv_nets(nw1, nwL1)
        expect_equiv_nets(nw0 + nw1, nwL0 + nwL1)
        expect_equiv_nets(nw0 - nw1, nwL0 - nwL1)
        expect_equiv_nets(nw0 + nw2, nwL0 + nwL2)
        expect_equiv_nets(nw0 - nw2, nwL0 - nwL2)
        expect_equiv_nets(nw2 + nw1, nwL2 + nwL1)
        expect_equiv_nets(nw2 - nw1, nwL2 - nwL1)

        set.seed(2)
        set.edge.attribute(nw0, "na", sample(c(FALSE, TRUE), network.edgecount(nw0, na.omit = FALSE), TRUE))
        set.seed(2)
        set.edge.attribute(nwL0, "na", sample(c(FALSE, TRUE), network.edgecount(nwL0, na.omit = FALSE), TRUE))
        expect_equiv_nets(nw0, nwL0)
        expect_equiv_nets(is.na(nw0), is.na(nwL0))
        expect_equiv_nets(is.na(is.na(nw0)), is.na(is.na(nwL0)))
        expect_equiv_nets(is.na(is.na(is.na(nw0))), is.na(is.na(is.na(nwL0))))
        expect_error(nwL0 + nwL1, "missing edges")
        expect_error(nwL0 - nwL1, "missing edges")
      }
    }
  }
})

test_that("initially atomic attribute assigns non-atomic values consistently", {
  nw <- network.initialize(5, directed = FALSE)
  nw %v% "verts" <- 1:5
  nwL <- as.networkLite(nw)
  expect_equiv_nets(nw, nwL)
  expect_true(is.atomic(nwL$attr$verts))
  set.vertex.attribute(nw, "verts", list(list(1),list(2:3),list(c("a","b"))), c(1,4,5))
  set.vertex.attribute(nwL, "verts", list(list(1),list(2:3),list(c("a","b"))), c(1,4,5))
  expect_equiv_nets(nw, nwL)
  expect_false(is.atomic(nwL$attr$verts))
  expect_identical(nwL$attr$verts, list(list(1), 2L, 3L, list(2:3), list(c("a","b"))))
  expect_identical(get.vertex.attribute(nwL, "verts", unlist = FALSE, null.na = FALSE),
                   list(list(1), 2L, 3L, list(2:3), list(c("a","b"))))
  expect_identical(get.vertex.attribute(nw, "verts", unlist = FALSE, null.na = FALSE),
                   list(list(1), 2L, 3L, list(2:3), list(c("a","b"))))
})

test_that("initialization errors or lack thereof", {
  el <- cbind(c(1,1,3,3,4), c(2,3,4,5,5))
  attr(el, "n") <- 5
  nw <- network(el, directed = FALSE)
  el <- as.edgelist(nw, output = "tibble")
  nwL <- networkLite(el)
  expect_equiv_nets(nw, nwL)
  names(el) <- c("tail", ".head")
  expect_error(networkLite(el), "'.tail' and '.head'")
  nw %e% "eattr" <- runif(5)
  el <- as.edgelist(nw, output = "tibble")
  nwL <- networkLite(el)
  expect_error(expect_equiv_nets(nw, nwL))
  el_attr <- as.edgelist(nw, attrname = "eattr", output = "tibble")
  nwL <- networkLite(el_attr)
  expect_equiv_nets(nw, nwL)
  delete.edge.attribute(nw, "eattr")
  set.edge.attribute(nw, "na", sample(c(FALSE,TRUE), 5, TRUE))
  el_attr <- as.edgelist(nw, attrname = "na", output = "tibble", na.rm = FALSE)
  nwL <- networkLite(el_attr)
  expect_equiv_nets(nw, nwL)  
  set.edge.attribute(nw, "na", FALSE)
  el <- as.edgelist(nw)
  el <- el[,1,drop=FALSE]
  attr(el, "n") <- 5
  expect_error(networkLite(el), "two columns")
})

test_that("more tibble tests", {
  nw <- network(create_random_edgelist(100L, FALSE, FALSE, 100L), directed = FALSE, bipartite = FALSE, matrix.type = "edgelist")
  nw %e% "e1" <- runif(network.edgecount(nw))
  nw %e% "e2" <- runif(network.edgecount(nw))
  nw %e% "na" <- sample(c(FALSE, TRUE), network.edgecount(nw), TRUE)
  
  tbl <- as_tibble(nw, na.rm = FALSE)
  tbl <- tbl[order(tbl$.tail, tbl$.head),]
  class(tbl) <- c("edgelist", class(tbl))
  nwL <- networkLite(tbl)
  expect_error(expect_equiv_nets(nw, nwL))
  expect_equal(list.edge.attributes(nwL), c("na"))

  tbl <- as_tibble(nw, attrnames = "e1", na.rm = FALSE)
  tbl <- tbl[order(tbl$.tail, tbl$.head),]
  class(tbl) <- c("edgelist", class(tbl))
  nwL <- networkLite(tbl)
  expect_error(expect_equiv_nets(nw, nwL))
  expect_equal(list.edge.attributes(nwL), c("e1", "na"))

  tbl <- as_tibble(nw, attrnames = c("e1", "e2"), na.rm = FALSE)
  tbl <- tbl[order(tbl$.tail, tbl$.head),]
  class(tbl) <- c("edgelist", class(tbl))
  nwL <- networkLite(tbl)
  expect_error(expect_equiv_nets(nw, nwL))
  expect_equal(list.edge.attributes(nwL), c("e1", "e2", "na"))

  tbl <- as_tibble(nw, attrnames = c("e1", "e2", "na"), na.rm = FALSE)
  tbl <- tbl[order(tbl$.tail, tbl$.head),]
  class(tbl) <- c("edgelist", class(tbl))
  nwL <- networkLite(tbl)
  expect_equiv_nets(nw, nwL)
  expect_equal(list.edge.attributes(nwL), c("e1", "e2", "na"))

  tbl <- as_tibble(nw, attrnames = c("e1", "e2", "na"), na.rm = TRUE)
  tbl <- tbl[order(tbl$.tail, tbl$.head),]
  class(tbl) <- c("edgelist", class(tbl))
  nwL <- networkLite(tbl)
  expect_error(expect_equiv_nets(nw, nwL))
  expect_equal(list.edge.attributes(nwL), c("e1", "e2", "na"))
})

test_that("direct conversion between network and networkLite functions as expected", {
  net_size <- 100
  bip_size <- 40
  edges_target <- 2*net_size

  for(directed in list(FALSE, TRUE)) {
    for(bipartite in list(FALSE, bip_size)) {
      if(directed && bipartite) {
        next
      }

      for(last.mode in list(FALSE, TRUE)) {
        for(delete in list(FALSE, TRUE)) {
          set.seed(0)
          nw <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")
          nw %v% "b" <- runif(net_size)
          nw %e% "eattr" <- runif(network.edgecount(nw))
          nw %n% "nattr" <- "attr"
          add.vertices(nw, 9, vattr = rep(list(list(na = FALSE, vertex.names = NA_integer_, b = NA_real_)), 9), last.mode = last.mode)
          set.edge.attribute(nw, "na", sample(c(FALSE, TRUE), network.edgecount(nw), TRUE))
          if(delete) {
            el <- as.edgelist(nw, attrname = "na", na.rm = FALSE)
            w1 <- sample(which(as.logical(el[,3])))[1:3]
            w2 <- sample(which(!as.logical(el[,3])))[1:4]
            delete.edges(nw, unlist(get.dyads.eids(nw, el[w1,1], el[w1,2], na.omit = FALSE)))
            delete.edges(nw, unlist(get.dyads.eids(nw, el[w2,1], el[w2,2], na.omit = FALSE)))
            vd <- sample(seq_len(net_size), 10, FALSE)
            delete.vertices(nw, vd)
          }

          set.seed(0)
          nwL <- networkLite(create_random_edgelist(net_size, directed, bipartite, edges_target))
          nwL %v% "b" <- runif(net_size)
          set.edge.attribute(nwL, "eattr", runif(network.edgecount(nwL)))
          nwL %n% "nattr" <- "attr"
          add.vertices(nwL, 9, vattr = rep(list(list(na = FALSE, vertex.names = NA_integer_, b = NA_real_)), 9), last.mode = last.mode)
          set.edge.attribute(nwL, "na", sample(c(FALSE, TRUE), network.edgecount(nwL), TRUE))
          if(delete) {
            el <- as.edgelist(nwL, attrname = "na", na.rm = FALSE)
            w1 <- sample(which(as.logical(el[,3])))[1:3]
            w2 <- sample(which(!as.logical(el[,3])))[1:4]
            delete.edges(nwL, c(w1,w2))
            vd <- sample(seq_len(net_size), 10, FALSE)
            delete.vertices(nwL, vd)
          }
        }

        expect_equiv_nets(as.networkLite(nw), nwL)
        expect_equiv_nets(as.networkLite(is.na(nw)), is.na(nwL))
        expect_equiv_nets(as.networkLite(is.na(is.na(nw))), is.na(is.na(nwL)))
        expect_equiv_nets(as.networkLite(is.na(is.na(is.na(nw)))), is.na(is.na(is.na(nwL))))

        expect_equiv_nets(nw, to_network_networkLite(nwL), skip.mnext = TRUE)
        expect_equiv_nets(is.na(nw), to_network_networkLite(is.na(nwL)), skip.mnext = TRUE)
      }
    }
  }
})

test_that("network and networkLite produce identical matrices, edgelists, and tibbles", {
  net_size <- 100
  bip_size <- 40
  edges_target <- net_size

  for(directed in list(FALSE, TRUE)) {
    for(bipartite in list(FALSE, bip_size)) {
      if(directed && bipartite) {
        next
      }

      set.seed(0)
      nw <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")
      nw %e% "eattr" <- runif(network.edgecount(nw))
      nw %e% "na" <- sample(c(FALSE, TRUE), network.edgecount(nw), TRUE)

      set.seed(0)
      nwL <- networkLite(create_random_edgelist(net_size, directed, bipartite, edges_target))
      set.edge.attribute(nwL, "eattr", runif(network.edgecount(nwL)))
      set.edge.attribute(nwL, "na", sample(c(FALSE, TRUE), network.edgecount(nwL), TRUE))

      for(attrname in list(NULL, "eattr", "na")) {
        for(na.rm in list(FALSE, TRUE)) {
          for(matrix.type in c("adjacency", "incidence", "edgelist")) {
            expect_identical(as.matrix(nw, matrix.type = matrix.type, attrname = attrname, na.rm = na.rm),
                             as.matrix(nwL, matrix.type = matrix.type, attrname = attrname, na.rm = na.rm))
          }
          expect_identical(as.edgelist(nw, attrname = attrname, na.rm = na.rm),
                           as.edgelist(nwL, attrname = attrname, na.rm = na.rm))

          expect_identical(as.edgelist(nw, attrname = attrname, na.rm = na.rm, output = "tibble"),
                           as.edgelist(nwL, attrname = attrname, na.rm = na.rm, output = "tibble"))

          expect_identical(tibble::as_tibble(nw, attrname = attrname, na.rm = na.rm),
                           tibble::as_tibble(nwL, attrname = attrname, na.rm = na.rm))
        }
      }
    }
  }
})

test_that("network and networkLite `[<-` and add.edges produce consistent edgelists", {
  net_size <- 100
  bip_size <- 40
  edges_target <- net_size

  for(directed in list(FALSE, TRUE)) {
    for(bipartite in list(FALSE, bip_size)) {
      if(directed && bipartite) {
        next
      }

      b1 <- if(bipartite) bip_size else net_size
      b2 <- if(bipartite) net_size - bip_size else net_size

      nw <- network.initialize(net_size, directed = directed, bipartite = bipartite)
      nwL <- networkLite(net_size, directed = directed, bipartite = bipartite)

      nwa <- nw
      nwLa <- nwL

      nw0 <- nw

      rv <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")
      m <- as.matrix(rv, matrix.type = "adjacency")
      el <- as.matrix(rv, matrix.type = "edgelist")

      nw[,] <- m
      nwL[,] <- m
      nwa <- add.edges(nwa, el[,1], el[,2])
      nwLa <- add.edges(nwLa, el[,1], el[,2])

      expect_equal(as.edgelist(nw), as.edgelist(nwL))
      expect_equal(as.edgelist(nwa), as.edgelist(nwLa))

      rv2 <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")
      m2 <- as.matrix(rv2, matrix.type = "adjacency")
      el2 <- as.matrix(rv2 - rv, matrix.type = "edgelist")

      nw[,] <- m2
      nwL[,] <- m2
      nwa <- add.edges(nwa, el2[,1], el2[,2])
      nwLa <- add.edges(nwLa, el2[,1], el2[,2])

      expect_equal(as.edgelist(nw), as.edgelist(nwL))
      expect_equal(as.edgelist(nwa), as.edgelist(nwLa))

      m <- matrix(runif(b1*b2), b1, b2)
      if(!directed && !bipartite) {
        m <- m + t(m)
      }

      m[m < 0.5] <- 0
      nwm <- network(m > 0, matrix.type = if(bipartite) "bipartite" else "adjacency", directed = directed, bipartite = bipartite)
      nwm[,,names.eval="w",add.edges=FALSE] <- m

      nwmd <- nwm - (rv + rv2)
      nwmd[,,names.eval="w",add.edges=FALSE] <- m
      elm <- as.edgelist(nwmd, attrname="w")

      nw[,,names.eval="w",add.edges=FALSE] <- m
      nwL[,,names.eval="w",add.edges=FALSE] <- m

      expect_equal(as.edgelist(nw, attrname = "w"), as.edgelist(nwL, attrname = "w"))

      nw[,,names.eval="w",add.edges=TRUE] <- m
      nwL[,,names.eval="w",add.edges=TRUE] <- m
      nwa <- add.edges(nwa, elm[,1], elm[,2], names.eval="w", vals.eval=elm[,3])
      nwLa <- add.edges(nwLa, elm[,1], elm[,2], names.eval="w", vals.eval=elm[,3])

      expect_equal(as.edgelist(nw, attrname = "w"), as.edgelist(nwL, attrname = "w"))
      expect_equal(as.edgelist(nwa, attrname = "w"), as.edgelist(nwLa, attrname = "w"))

      nw[,] <- FALSE
      nwL[,] <- FALSE
      nwa[,] <- FALSE
      nwLa[,] <- FALSE

      elm2 <- as.edgelist(nwm, attrname="w")

      nw[,,names.eval="w",add.edges=TRUE] <- m
      nwL[,,names.eval="w",add.edges=TRUE] <- m

      nwa <- add.edges(nwa, elm2[,1], elm2[,2], names.eval="w", vals.eval=elm2[,3])
      nwLa <- add.edges(nwLa, elm2[,1], elm2[,2], names.eval="w", vals.eval=elm2[,3])

      expect_equal(as.edgelist(nw, attrname = "w"), as.edgelist(nwL, attrname = "w"))
      expect_equal(as.edgelist(nwa, attrname = "w"), as.edgelist(nwLa, attrname = "w"))
    }
  }
})

test_that("network and networkLite `+` and `-` produce consistent results", {
  net_size <- 100
  bip_size <- 40
  edges_target <- 10*net_size

  for(directed in list(FALSE, TRUE)) {
    for(bipartite in list(FALSE, bip_size)) {
      if(directed && bipartite) {
        next
      }

      nw1 <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")
      nw2 <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")

      nwL1 <- as.networkLite(nw1)
      nwL2 <- as.networkLite(nw2)

      expect_identical(as.edgelist(nw1 + nw2), as.edgelist(nwL1 + nwL2))
      expect_identical(as.edgelist(nw1 - nw2), as.edgelist(nwL1 - nwL2))
    }
  }
})

test_that("network to networkLite conversion handles deleted edges with attributes appropriately", {
  net_size <- 10
  bip_size <- 4

  for(directed in list(FALSE, TRUE)) {
    for(bipartite in list(FALSE, bip_size)) {
      if(directed && bipartite) {
        next
      }

      nw <- network.initialize(net_size, directed = directed, bipartite = bipartite)
      nw[1,5] <- 1
      nw[4,7] <- 1
      nw[3,9] <- 1
      nw[2,6] <- 1
      nw[1,10] <- 1
      eattr <- runif(5)
      nw %e% "eattr" <- eattr
      delete.edges(nw, c(2,4))

      nwL <- as.networkLite(nw)
      expect_identical(nw %e% "eattr", eattr[c(1,3,5)])
      expect_identical(nwL %e% "eattr", eattr[c(1,5,3)])
      expect_identical(as.edgelist(nw), as.edgelist(nwL))
    }
  }
})

test_that("network and networkLite behave equivalently for basic access and mutation", {
  net_size <- 10
  bip_size <- 4

  for(directed in list(FALSE, TRUE)) {
    for(bipartite in list(FALSE, bip_size)) {
      if(directed && bipartite) {
        next
      }

      nw <- network.initialize(net_size, directed = directed, bipartite = bipartite)
      nw[1,5] <- 1
      nw[4,7] <- 1
      nw[3,9] <- 1
      nw[2,6] <- 1
      nw[1,10] <- 1
      eattr1 <- runif(3)
      e1 <- c(2,1,4)
      eattr2 <- "a"
      e2 <- c(2,4)
      eattr3 <- runif(5)

      set.edge.attribute(nw, "eattr1", eattr1, e1)
      set.edge.attribute(nw, "eattr2", eattr2, e2)
      nw %e% "eattr3" <- eattr3

      vattr1 <- sample(c("a","b"), 7, TRUE)
      v1 <- c(5,9,4,6,8,1,2)
      vattr2 <- FALSE
      v2 <- c(8)
      vattr3 <- runif(10)

      set.vertex.attribute(nw, "vattr1", vattr1, v1)
      set.vertex.attribute(nw, "vattr2", vattr2, v2)
      nw %v% "vattr3" <- vattr3

      nwL <- networkLite(as.edgelist(nw))

      eo <- c(1,5,4,3,2)

      set.edge.attribute(nwL, "eattr1", eattr1, eo[e1])
      set.edge.attribute(nwL, "eattr2", eattr2, eo[e2])
      set.edge.attribute(nwL, "eattr3", eattr3[eo])

      set.vertex.attribute(nwL, "vattr1", vattr1, v1)
      set.vertex.attribute(nwL, "vattr2", vattr2, v2)
      nwL %v% "vattr3" <- vattr3

      expect_identical(as.edgelist(nw, attrname = "eattr1"), as.edgelist(nwL, attrname = "eattr1"))
      expect_identical(as.edgelist(nw, attrname = "eattr2"), as.edgelist(nwL, attrname = "eattr2"))
      expect_identical(as.edgelist(nw, attrname = "eattr3"), as.edgelist(nwL, attrname = "eattr3"))

      expect_identical(nw %v% "vattr1", nwL %v% "vattr1")
      expect_identical(nw %v% "vattr2", nwL %v% "vattr2")
      expect_identical(nw %v% "vattr3", nwL %v% "vattr3")
    }
  }
})

test_that("add.vertices and add.edges with irregular attribute arguments behave equivalently for network and networkLite", {
  net_size <- 100
  bip_size <- 40
  edges_target <- net_size

  for(directed in list(FALSE, TRUE)) {
    for(bipartite in list(FALSE, bip_size)) {
      if(directed && bipartite) {
        next
      }

      for(last.mode in list(FALSE, TRUE)) {

        vnames <- paste0("v", 1:4)
        enames <- paste0("e", 1:4)

        set.seed(0)
        nwe <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")
        nw <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")
        nw %v% "v1" <- runif(net_size)
        nw %v% "v2" <- runif(net_size)
        nw %e% "e1" <- runif(network.edgecount(nw))
        nw %e% "e2" <- runif(network.edgecount(nw))
        el <- as.edgelist(nwe - nw)
        names.eval <- list()
        vals.eval <- list()
        for(i in seq_len(NROW(el))) {
          en <- which(as.logical(round(runif(length(enames)))))
          if(length(en) > 0) {
            en <- sample(en)
            names.eval[[i]] <- as.list(enames[en])
            vals.eval[[i]] <- as.list(runif(length(en)))
          } else {
            names.eval[[i]] <- list()
            vals.eval[[i]] <- list()
          }
        }
        add.edges(nw, el[,1], el[,2], names.eval = names.eval, vals.eval = vals.eval)

        vta <- 50
        vattr <- list()
        for(i in seq_len(vta)) {
          vn <- which(as.logical(round(runif(length(vnames)))))
          if(length(vn) > 0) {
            vn <- sample(vn)
            vattr[[i]] <- as.list(runif(length(vn)))
            names(vattr[[i]]) <- vnames[vn]
          } else {
            vattr[[i]] <- list()
          }
        }
        add.vertices(nw, vta, vattr = vattr, last.mode = last.mode)

        set.seed(0)
        nwLe <- networkLite(create_random_edgelist(net_size, directed, bipartite, edges_target))
        nwL <- networkLite(create_random_edgelist(net_size, directed, bipartite, edges_target))
        nwL %v% "v1" <- runif(net_size)
        nwL %v% "v2" <- runif(net_size)
        set.edge.attribute(nwL, "e1", runif(network.edgecount(nwL)))
        set.edge.attribute(nwL, "e2", runif(network.edgecount(nwL)))
        el <- as.edgelist(nwLe - nwL)

        names.eval <- list()
        vals.eval <- list()
        for(i in seq_len(NROW(el))) {
          en <- which(as.logical(round(runif(length(enames)))))
          if(length(en) > 0) {
            en <- sample(en)
            names.eval[[i]] <- as.list(enames[en])
            vals.eval[[i]] <- as.list(runif(length(en)))
          } else {
            names.eval[[i]] <- list()
            vals.eval[[i]] <- list()
          }
        }
        add.edges(nwL, el[,1], el[,2], names.eval = names.eval, vals.eval = vals.eval)

        vta <- 50
        vattr <- list()
        for(i in seq_len(vta)) {
          vn <- which(as.logical(round(runif(length(vnames)))))
          if(length(vn) > 0) {
            vn <- sample(vn)
            vattr[[i]] <- as.list(runif(length(vn)))
            names(vattr[[i]]) <- vnames[vn]
          } else {
            vattr[[i]] <- list()
          }
        }
        add.vertices(nwL, vta, vattr = vattr, last.mode = last.mode)

        expect_equiv_nets(as.networkLite(nw), nwL)
        expect_equiv_nets(as.networkLite(nw), as.networkLite(to_network_networkLite(nwL)))
      }
    }
  }
})

test_that("attribute setting and deleting behave equivalently for network and networkLite", {
  net_size <- 100
  bip_size <- 40
  edges_target <- net_size

  enames <- paste0("e", 1:10)
  vnames <- paste0("v", 1:10)
  nnames <- paste0("n", 1:10)
  niter <- 100

  for(directed in list(FALSE, TRUE)) {
    for(bipartite in list(FALSE, bip_size)) {
      if(directed && bipartite) {
        next
      }

      set.seed(0)
      nw <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")
      for(i in seq_len(niter)) {
        en <- sample(enames, 1)
        vn <- sample(vnames, 1)
        nn <- sample(nnames, 1)

        if(en %in% list.edge.attributes(nw)) {
          delete.edge.attribute(nw, en)
        } else {
          set.edge.attribute(nw, en, runif(network.edgecount(nw)))
        }
        if(vn %in% list.vertex.attributes(nw)) {
          delete.vertex.attribute(nw, vn)
        } else {
          set.vertex.attribute(nw, vn, runif(network.size(nw)))
        }
        if(nn %in% list.network.attributes(nw)) {
          delete.network.attribute(nw, nn)
        } else {
          set.network.attribute(nw, nn, runif(1))
        }
      }

      set.seed(0)
      nwL <- networkLite(create_random_edgelist(net_size, directed, bipartite, edges_target))
      for(i in seq_len(niter)) {
        en <- sample(enames, 1)
        vn <- sample(vnames, 1)
        nn <- sample(nnames, 1)

        if(en %in% list.edge.attributes(nwL)) {
          delete.edge.attribute(nwL, en)
        } else {
          set.edge.attribute(nwL, en, runif(network.edgecount(nwL)))
        }
        if(vn %in% list.vertex.attributes(nwL)) {
          delete.vertex.attribute(nwL, vn)
        } else {
          set.vertex.attribute(nwL, vn, runif(network.size(nwL)))
        }
        if(nn %in% list.network.attributes(nwL)) {
          delete.network.attribute(nwL, nn)
        } else {
          set.network.attribute(nwL, nn, runif(1))
        }
      }

      expect_equiv_nets(nw, to_network_networkLite(nwL))
      expect_equiv_nets(as.networkLite(nw), nwL)
    }
  }
})

test_that("as.networkLite conversion errors work as expected with respect to network attributes", {
  net_size <- 10L
  bip_size <- 4L

  nw <- network.initialize(net_size)
  nwL <- as.networkLite(nw)
  expect_is(nwL, "networkLite")
  expect_is(nwL, "network")

  nw <- network.initialize(net_size, directed = FALSE)
  nwL <- as.networkLite(nw)
  expect_is(nwL, "networkLite")
  expect_is(nwL, "network")

  nw <- network.initialize(net_size, directed = FALSE, bipartite = bip_size)
  nwL <- as.networkLite(nw)
  expect_is(nwL, "networkLite")
  expect_is(nwL, "network")

  nw <- network.initialize(net_size, directed = FALSE, bipartite = FALSE, hyper = TRUE)
  expect_error(nwL <- as.networkLite(nw), "cannot coerce `network` to `networkLite`")

  nw <- network.initialize(net_size, directed = FALSE, bipartite = FALSE, multiple = TRUE)
  expect_error(nwL <- as.networkLite(nw), "cannot coerce `network` to `networkLite`")

  nw <- network.initialize(net_size, directed = FALSE, bipartite = FALSE, loops = TRUE)
  expect_error(nwL <- as.networkLite(nw), "cannot coerce `network` to `networkLite`")
})

test_that("more network conversions", {
  nw <- network.initialize(5, directed = FALSE)
  nwL <- networkLite(5)

  set.vertex.attribute(nw, "newattr", list(1,list(3),5), c(3,4,1))
  set.vertex.attribute(nwL, "newattr", list(1,list(3),5), c(3,4,1))

  expect_identical(get.vertex.attribute(nw, "newattr", null.na = TRUE, unlist = FALSE),
                   get.vertex.attribute(nwL, "newattr", null.na = TRUE, unlist = FALSE))

  expect_identical(get.vertex.attribute(nw, "newattr", null.na = TRUE, unlist = TRUE),
                   get.vertex.attribute(nwL, "newattr", null.na = TRUE, unlist = TRUE))

  nw[1,2] <- 1
  nw[3,4] <- 1
  nw[2,5] <- 1

  set.edge.attribute(nw, "eattr", list(list(NULL), NA, list(3)))

  nwL <- as.networkLite(nw)
  el <- as.edgelist(nwL)

  eids <- unlist(get.dyads.eids(nw, el[,1], el[,2]))
  expect_identical(get.edge.attribute(nw, "eattr", null.na = TRUE, unlist = FALSE)[eids],
                   get.edge.attribute(nwL, "eattr", null.na = TRUE, unlist = FALSE))
  expect_identical(unlist(get.edge.attribute(nw, "eattr", null.na = TRUE, unlist = FALSE)[eids]),
                   unlist(get.edge.attribute(nwL, "eattr", null.na = TRUE, unlist = FALSE)))
})

test_that("as.edgelist with attrname", {
  nw <- network.initialize(10, directed = FALSE)
  nw[1,2] <- 1
  nw[1,5] <- 1
  nw[2,7] <- 1
  nw[3,8] <- 1
  nw[5,10] <- 1

  nwL <- networkLite(as.edgelist(nw))

  set.edge.attribute(nwL, "eattr", list(1, 2, NULL, NA, 3))

  el <- as.edgelist(nwL, attrname = "eattr")

  expect_equal(nwL$el[["eattr"]], list(1, 2, NULL, NA, 3))
  expect_equal(el[,3], c(1, 2, NA, NA, 3))
})
