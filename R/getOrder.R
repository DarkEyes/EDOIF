#' getOrder function
#'
#' getOrder is a support function for inferring a linear order of categories ascendingly sorted by their means.
#'
#'@param Values is a vector of real-number values
#'@param Group is a vector of categories of each real number in Values
#'
#'@return This function returns two lists: an order list of categories \code{sortedGroupList}
#'and its correspoding list of means \code{sortedmeanList}.
#'
#'\item{sortedGroupList}{ The list of names of categories ascendingly ordered by their means. }
#'\item{sortedmeanList}{ The list of means of categories that are ascendingly ordered. }
#'
#'@examples
#'# Generate simulation data
#'
#'simData<-SimNonNormalDist(nInv=100,noisePer=0.1)
#'
#'# Call the function to get the sorted lists
#'getOrder(Values=simData$Values,Group=simData$Group)
#'
#'@export
#'
getOrder<- function(Values,Group) {
  GroupList<-unique(Group)
  meanList<-c()
  sortedMeanRatioList<-c()
  for(gr in GroupList)
  {
    meanList<-c(meanList,mean(Values[Group==gr]))
  }
  sortedInx<-order(meanList)
  sortedGroupList<-GroupList[sortedInx]
  sortedmeanList<-meanList[sortedInx]

  return(list("sortedGroupList"=sortedGroupList,"sortedmeanList"=sortedmeanList))
}
