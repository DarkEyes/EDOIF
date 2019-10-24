
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
