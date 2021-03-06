#' getiGraphNetDen function
#'
#' getiGraphNetDen is a support function for calculating a network density of a dominant-distribution network.
#'
#' @param g is an object of iGraph class of a dominant-distribution network.
#'
#' @return This function returns a value of network density of of a dominant-distribution network for a given object g.
#'
#' @examples
#' # Generate simulation data with 100 samples per categories
#'
#' simData<-SimNonNormalDist(nInv=100)
#'
#' # Performing ordering infernce from simData
#'
#' resultObj<-EDOIF(simData$Values,simData$Group)
#'
#' # Get a network density of an iGraph object
#'
#' getiGraphNetDen(g=resultObj$gObj)
#'
#'@export
#'
getiGraphNetDen<-function(g)
{
  n<-length(igraph::V(g))
  netDen<-length(igraph::E(g))/(n*(n-1)/2)
  return(netDen)
}

#' getADJNetDen function
#'
#' getADJNetDen is a support function for calculating a network density of a dominant-distribution network.
#'
#' @param adjMat is an adjacency matrix of a dominant-distribution network.
#'
#' @return This function returns a value of network density of of a dominant-distribution network for a given adjMat.
#'
#' @examples
#' # Generate simulation data with 100 samples per categories
#'
#' simData<-SimNonNormalDist(nInv=100)
#'
#' # Performing ordering infernce from simData
#'
#' resultObj<-EDOIF(simData$Values,simData$Group)
#'
#' # Get a network density of an adjacency matrix
#'
#' getADJNetDen(adjMat=resultObj$adjMat)
#'
#'@export
#'
getADJNetDen<-function(adjMat)
{
  n<-dim(adjMat)[1]
  netDen<-sum(adjMat)/(n*(n-1)/2)
  return(netDen)
}

#' getiGraphOBJ function
#'
#' getiGraphOBJ is a support function for converting a dominant-distribution network adjacency matrix to an iGraph object.
#'
#' @param adjMat is an adjacency matrix of a dominant-distribution network.
#' @param sortedGroupList is a list of names of categories ascendingly ordered by their means.
#'
#' @return This function returns an iGraph object of a dominant-distribution network for a given adjMat.
#'
#' @examples
#' # Generate simulation data with 100 samples per categories
#'
#' simData<-SimNonNormalDist(nInv=100)
#'
#' # Performing ordering infernce from simData
#'
#' resultObj<-EDOIF(simData$Values,simData$Group)
#'
#' # Get an iGraph object from an adjacency matrix
#'
#' igraphObj<-getiGraphOBJ(adjMat=resultObj$adjMat,sortedGroupList=resultObj$sortedGroupList)
#'
#'@export
#'
getiGraphOBJ<-function(adjMat,sortedGroupList)
{
  g1 <- graph_from_adjacency_matrix( adjMat   ) %>%
    set_vertex_attr("label", value = sortedGroupList)
  return(g1)
}
