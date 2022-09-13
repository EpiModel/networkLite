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

test_that("direct conversion between network and networkLite functions as expected", {
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
        nw <- network(create_random_edgelist(net_size, directed, bipartite, edges_target), directed = directed, bipartite = bipartite, matrix.type = "edgelist")
        nw %v% "b" <- runif(net_size)
        nw %e% "eattr" <- runif(network.edgecount(nw))
        nw %n% "nattr" <- "attr"
        add.vertices(nw, 9, vattr = rep(list(list(na = FALSE, vertex.names = NA_integer_, b = NA_real_)), 9), last.mode = last.mode)

        set.seed(0)
        nwL <- networkLite(create_random_edgelist(net_size, directed, bipartite, edges_target))
        nwL %v% "b" <- runif(net_size)
        set.edge.attribute(nwL, "eattr", runif(network.edgecount(nwL)))
        nwL %n% "nattr" <- "attr"
        add.vertices(nwL, 9, vattr = rep(list(list(na = FALSE, vertex.names = NA_integer_, b = NA_real_)), 9), last.mode = last.mode)

        expect_equiv_nets(as.networkLite(nw), nwL)
        expect_equiv_nets(as.networkLite(is.na(nw)), is.na(nwL))
        expect_equiv_nets(as.networkLite(is.na(is.na(nw))), is.na(is.na(nwL)))

        expect_equiv_nets(nw, to_network_networkLite(nwL))
        expect_equiv_nets(is.na(nw), to_network_networkLite(is.na(nwL)))
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

      set.seed(0)
      nwL <- networkLite(create_random_edgelist(net_size, directed, bipartite, edges_target))
      set.edge.attribute(nwL, "eattr", runif(network.edgecount(nwL)))

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
#      expect_identical(as.edgelist(nw, attrname = "eattr2"), as.edgelist(nwL, attrname = "eattr2"))
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
