#' getOrder function
#'
#' getOrder is a support function for inferring a linear order of categories ascendingly sorted by their means.
#'
#'@param Values is a vector of real-number values
#'@param Group is a vector of categories of each real number in Values
#'
#'@return This function returns an order list of categories.
#'
#'\item{sortedGroupList}{ The list of names of categories ascendingly ordered by their means. }
#'\item{sortedmeanList}{ The list of means of categories that are ascendingly ordered. }
#'
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
  for(i in seq(2,length(sortedmeanList)) )
  {
    sortedMeanRatioList<-c(sortedMeanRatioList,sortedmeanList[i-1]/sortedmeanList[i])
  }
  return(list("sortedGroupList"=sortedGroupList,"sortedmeanList"=sortedmeanList,"sortedMeanRatioList"=sortedMeanRatioList))
}
