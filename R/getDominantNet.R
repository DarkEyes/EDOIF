getDominantRADJ<-function(MegDiffList,methodType)
{
  N<-length(MegDiffList)+1
  adjMat<-matrix(0,N,N)
  adjDiffMat<-matrix(0,N,N)
  for(j in seq(1,N-1))
  {
    if(methodType == "bca")
      CIlowerBoundCurr<- MegDiffList[[j]]$result$bca_ci_low
    else
      CIlowerBoundCurr<- MegDiffList[[j]]$result$ci_low
    M<-length(CIlowerBoundCurr)
    for(i in seq(1,M))
    {
      if( CIlowerBoundCurr[i]>0)
      {
        adjMat[i+j,j]<-1
        adjDiffMat[i+j,j]<-CIlowerBoundCurr[i]
      }
    }
  }
  return(list("adjMat"=adjMat, "adjDiffMat"=adjDiffMat))
}
getWilcoxDominantRADJ<-function(Values,Group,GroupList,alpha)
{
  if(missing(alpha)) {
    alpha = 0.05
  }
  N<-length(GroupList)
  adjMat<-matrix(0,N,N)
  pValMat<-matrix(0,N,N)

  for(i in seq(2,N))
  {
    for(j in seq(1,i-1))
    {

      out<-wilcox.test(Values[Group == GroupList[i]], Values[Group == GroupList[j]],p.adjust.method = "BY", alternative = "greater" )
      pValMat[i,j]<-out$p.value
      if(  out$p.value <= alpha )
      {
        adjMat[i,j]<-1
      }
    }
  }
  return(list("adjMat"=adjMat,"pValMat"=pValMat))
}
getttestDominantRADJ<-function(Values,Group,GroupList,alpha)
{
  if(missing(alpha)) {
    alpha = 0.05
  }
  N<-length(GroupList)
  adjMat<-matrix(0,N,N)
  pValMat<-matrix(0,N,N)

  for(i in seq(2,N))
  {
    for(j in seq(1,i-1))
    {

      out<-t.test(Values[Group == GroupList[i]], Values[Group == GroupList[j]],p.adjust.method = "BY", alternative = "greater" )
      pValMat[i,j]<-out$p.value
      if(  out$p.value <= alpha )
      {
        adjMat[i,j]<-1
      }
    }
  }
  return(list("adjMat"=adjMat,"pValMat"=pValMat))
}
