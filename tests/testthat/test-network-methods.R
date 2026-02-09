library(testthat)
library(network)

test_that("get.edgeIDs works for networkLite", {
  # Create a simple directed network
  nw <- networkLite(5, directed = TRUE)
  add.edges(nw, c(1, 2, 3), c(2, 3, 4))
  
  # Test getting edge IDs
  eid1 <- get.edgeIDs(nw, 1, 2)
  expect_equal(length(eid1), 1)
  expect_true(eid1 > 0)
  
  eid2 <- get.edgeIDs(nw, 2, 3)
  expect_equal(length(eid2), 1)
  expect_true(eid2 > 0)
  
  # Test non-existent edge
  eid_none <- get.edgeIDs(nw, 1, 5)
  expect_equal(length(eid_none), 0)
  
  # Test undirected network
  nw_undir <- networkLite(5, directed = FALSE)
  add.edges(nw_undir, c(1, 2), c(2, 3))
  
  # For undirected, order shouldn't matter
  eid_a <- get.edgeIDs(nw_undir, 1, 2)
  eid_b <- get.edgeIDs(nw_undir, 2, 1)
  expect_equal(eid_a, eid_b)
})

test_that("get.edgeIDs handles missing edges", {
  nw <- networkLite(5, directed = TRUE)
  add.edges(nw, c(1, 2), c(2, 3), names.eval = list("na", "na"), 
            vals.eval = list(TRUE, FALSE))
  
  # With na.omit = TRUE, missing edge should not be returned
  eid1 <- get.edgeIDs(nw, 1, 2, na.omit = TRUE)
  expect_equal(length(eid1), 0)
  
  # With na.omit = FALSE, missing edge should be returned
  eid2 <- get.edgeIDs(nw, 1, 2, na.omit = FALSE)
  expect_equal(length(eid2), 1)
  
  # Non-missing edge should be returned either way
  eid3 <- get.edgeIDs(nw, 2, 3, na.omit = TRUE)
  expect_equal(length(eid3), 1)
  eid4 <- get.edgeIDs(nw, 2, 3, na.omit = FALSE)
  expect_equal(length(eid4), 1)
})

test_that("get.dyads.eids works for networkLite", {
  nw <- networkLite(5, directed = TRUE)
  add.edges(nw, c(1, 2, 3), c(2, 3, 4))
  
  # Test getting multiple edge IDs
  eids <- get.dyads.eids(nw, c(1, 2, 3), c(2, 3, 4))
  expect_equal(length(eids), 3)
  expect_true(all(sapply(eids, length) == 1))
  
  # Test with some non-existent edges
  eids2 <- get.dyads.eids(nw, c(1, 1, 2), c(2, 5, 3))
  expect_equal(length(eids2), 3)
  expect_equal(length(eids2[[1]]), 1)  # exists
  expect_equal(length(eids2[[2]]), 0)  # doesn't exist
  expect_equal(length(eids2[[3]]), 1)  # exists
})

test_that("get.edges works for networkLite", {
  nw <- networkLite(5, directed = TRUE)
  add.edges(nw, c(1, 2, 3, 1), c(2, 3, 4, 3))
  
  # Test getting edges incident on a vertex
  edges1 <- get.edges(nw, v = 1)
  expect_true(length(edges1) > 0)
  
  # Test getting outgoing edges
  edges_out <- get.edges(nw, v = 1, neighborhood = "out")
  expect_true(length(edges_out) > 0)
  
  # Test getting incoming edges
  edges_in <- get.edges(nw, v = 3, neighborhood = "in")
  expect_true(length(edges_in) > 0)
  
  # Test getting edge between two specific vertices
  edges_between <- get.edges(nw, v = 1, alter = 2)
  expect_equal(length(edges_between), 1)
})

test_that("get.neighborhood works for networkLite", {
  nw <- networkLite(5, directed = TRUE)
  add.edges(nw, c(1, 2, 3), c(2, 3, 4))
  
  # Test getting neighbors
  neighbors1 <- get.neighborhood(nw, 1)
  expect_true(2 %in% neighbors1)
  
  # Test out-neighbors
  neighbors_out <- get.neighborhood(nw, 2, type = "out")
  expect_true(3 %in% neighbors_out)
  
  # Test in-neighbors
  neighbors_in <- get.neighborhood(nw, 3, type = "in")
  expect_true(2 %in% neighbors_in)
  
  # Test undirected network
  nw_undir <- networkLite(5, directed = FALSE)
  add.edges(nw_undir, c(1, 2), c(2, 3))
  neighbors_undir <- get.neighborhood(nw_undir, 2)
  expect_true(1 %in% neighbors_undir)
  expect_true(3 %in% neighbors_undir)
})

test_that("is.adjacent works for networkLite", {
  nw <- networkLite(5, directed = TRUE)
  add.edges(nw, c(1, 2, 3), c(2, 3, 4))
  
  # Test existing edge
  expect_true(is.adjacent(nw, 1, 2))
  expect_true(is.adjacent(nw, 2, 3))
  
  # Test non-existent edge
  expect_false(is.adjacent(nw, 1, 5))
  expect_false(is.adjacent(nw, 5, 1))
  
  # Test directed network - order matters
  expect_true(is.adjacent(nw, 1, 2))
  expect_false(is.adjacent(nw, 2, 1))
  
  # Test undirected network - order doesn't matter
  nw_undir <- networkLite(5, directed = FALSE)
  add.edges(nw_undir, c(1, 2), c(2, 3))
  expect_true(is.adjacent(nw_undir, 1, 2))
  expect_true(is.adjacent(nw_undir, 2, 1))
})

test_that("network.density works for networkLite", {
  # Test directed network
  nw_dir <- networkLite(5, directed = TRUE)
  expect_equal(network.density(nw_dir), 0)
  
  add.edges(nw_dir, c(1, 2), c(2, 3))
  # 2 edges out of 5*4 = 20 possible
  expect_equal(network.density(nw_dir), 2 / 20)
  
  # Test undirected network
  nw_undir <- networkLite(5, directed = FALSE)
  add.edges(nw_undir, c(1, 2), c(2, 3))
  # 2 edges out of 5*4/2 = 10 possible
  expect_equal(network.density(nw_undir), 2 / 10)
  
  # Test bipartite network
  nw_bip <- networkLite(6, directed = FALSE, bipartite = 3)
  add.edges(nw_bip, c(1, 2), c(4, 5))
  # 2 edges out of 3*3 = 9 possible
  expect_equal(network.density(nw_bip), 2 / 9)
  
  # Test empty network
  nw_empty <- networkLite(0)
  expect_true(is.nan(network.density(nw_empty)))
})

test_that("has.edges works for networkLite", {
  nw <- networkLite(5, directed = TRUE)
  
  # Empty network
  expect_false(has.edges(nw))
  
  # Network with edges
  add.edges(nw, c(1, 2), c(2, 3))
  expect_true(has.edges(nw))
  
  # Network with only missing edges
  nw2 <- networkLite(5, directed = TRUE)
  add.edges(nw2, c(1), c(2), names.eval = list("na"), vals.eval = list(TRUE))
  expect_false(has.edges(nw2))
})

test_that("methods work together with network package", {
  # Create a network using the network package
  nw_net <- network.initialize(5, directed = TRUE)
  add.edges(nw_net, c(1, 2, 3), c(2, 3, 4))
  
  # Convert to networkLite
  nw_lite <- as.networkLite(nw_net)
  
  # Test that edge IDs work consistently
  eid_net <- get.edgeIDs(nw_net, 1, 2)
  eid_lite <- get.edgeIDs(nw_lite, 1, 2)
  expect_equal(length(eid_net), length(eid_lite))
  
  # Test density
  expect_equal(network.density(nw_net), network.density(nw_lite))
  
  # Test adjacency
  expect_equal(is.adjacent(nw_net, 1, 2), is.adjacent(nw_lite, 1, 2))
  expect_equal(is.adjacent(nw_net, 2, 1), is.adjacent(nw_lite, 2, 1))
  
  # Test has.edges
  expect_equal(has.edges(nw_net), has.edges(nw_lite))
})
