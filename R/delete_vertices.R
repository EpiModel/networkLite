
#' @rdname delete_vertices
#'
#' @title Delete vertices from a networkLite.
#'
#' @param x A `networkLite` object.
#' @param vid Vertex ids (between `1` and `network.size(x)`) to delete
#'            from `x`. Note that edges involving deleted vertices will
#'            also be deleted.
#' @param ... additional arguments.
#'
#' @return A `networkLite` object with the specified vertices deleted.
#'
#' @export
#'
delete.vertices.networkLite <- function(x, vid, ...) {
  vid <- as.integer(vid)
  vid <- vid[vid >= 1 & vid <= network.size(x)]
  if (length(vid) > 0) {
    # drop edges with deleted nodes
    x$el <- x$el[!(x$el$.tail %in% vid | x$el$.head %in% vid), ]

    # drop vertex attributes for deleted nodes
    x$attr <- x$attr[-vid, ]

    # remap nodal indices for remaining edges
    a <- seq_len(network.size(x))
    b <- integer(network.size(x))
    b[vid] <- 1L
    b <- cumsum(b)
    a <- a - b
    x$el$.tail <- a[x$el$.tail]
    x$el$.head <- a[x$el$.head]

    # update network attributes
    x %n% "n" <- x %n% "n" - length(vid)
    if (is.bipartite(x)) {
      x %n% "bipartite" <- x %n% "bipartite" - sum(vid <= x %n% "bipartite")
    }
  }

  modify_in_place(x)
}


#' Return an induced subgraph
#'
#' @param x,v,alters,... see [network::get.inducedSubgraph()]
#' @export
get.inducedSubgraph.networkLite <- function(x, v, alters=NULL, ...){
  #Do some reality checking
  n<-network.size(x)

  # do checks for v and alters
  if((length(v)<1)||any(is.na(v))||any(v<1)||any(v>n))
    stop("Illegal vertex selection in get.inducedSubgraph")
  if(!is.null(alters)){
    if((length(alters)<1)||any(is.na(alters))||any(alters<1)||any(alters>n)|| any(alters%in%v))
      stop("Illegal vertex selection (alters) in get.inducedSubgraph")
  }

  #Start by making a copy of our target network (yes, this can be wasteful)
  #TODO: in most cases, probably faster to create a new network and only copy over what is needed

  #Now, strip out what is needed, and/or permute in the two-mode case
  if(is.null(alters)){                    #Simple case
    delete.vertices(x,(1:n)[-v])           #Get rid of everyone else
  }else{                                  #Really an edge cut, but w/vertices
    nv<-length(v)
    na<-length(alters)
    newids<-sort(c(v,alters))
    newv<-match(v,newids)
    newalt<-match(alters,newids)
    delete.vertices(x,(1:n)[-c(v,alters)])  #Get rid of everyone else
    permute.vertexIDs(x,c(newv,newalt))    #Put the new vertices first
    #Remove within-group edges
    x$el <- x$el[(x$el$.tail <= nv) != (x$el$.head <= nv), , drop=FALSE]
    x%n%"bipartite"<-nv   #Set bipartite attribute
  }

  x
}
