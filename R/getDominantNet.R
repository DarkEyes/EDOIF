#' getDominantRADJ function
#'
#' getDominantRADJ is a support function for inferring a dominant-distribution network using mean-difference confidence intervals.
#'
#'@param MegDiffList is a list of objects that contains mean-difference confidence intervals inferred by getMegDiffConfInv function.
#'@param methodType is an option for boostrapping methods:either  "perc" or "bca".
#'
#'@return This function returns an adjacency matrix of a dominant-distribution network \code{adjMat}
#' and the corresponding lower-bound of mean difference CIs \code{adjDiffMat}.
#'
#'\item{adjDiffMat[i,j]}{ A lower bound of confidence interval of mean difference for j minus i using methodType bootstrap. }
#'\item{adjMat[i,j]}{ An element of adjacency matrix: One if adjDiffMat[i,j] is positive, otherwise, zero. }
#'
#'
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
#' getWilcoxDominantRADJ function
#'
#' getWilcoxDominantRADJ is a support function for inferring a dominant-distribution network using Mann-Whitney (Wilcoxon) Test.
#'
#'@param Values is a vector of real-number values
#'@param Group is a vector of categories of each real number in Values
#'@param GroupList is a list of names of categories ascendingly ordered by their means.
#'@param alpha is a significance level using in both confidence intervals and ordering inference it has the range [0,1].
#'
#'@return This function returns an adjacency matrix of a dominant-distribution network \code{adjMat}
#' and the corresponding p-values of all category pairs.
#'
#'\item{adjMat[i,j]}{ An element of adjacency matrix: one if GroupList[j] category dominates GroupList[i] using Mann-Whitney test, otherwise zero. }
#'\item{pValMat[i,j]}{ A p-value of Mann-Whitney test for adjMat[i,j]. }
#'
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
#' getttestDominantRADJ function
#'
#' getttestDominantRADJ is a support function for inferring a dominant-distribution network using Student's t-test.
#'
#'@param Values is a vector of real-number values
#'@param Group is a vector of categories of each real number in Values
#'@param GroupList is a list of names of categories ascendingly ordered by their means.
#'@param alpha is a significance level using in both confidence intervals and ordering inference it has the range [0,1].
#'
#'@return This function returns an adjacency matrix of a dominant-distribution network \code{adjMat}
#' and the corresponding p-values of all category pairs.
#'
#'\item{adjMat[i,j]}{ An element of adjacency matrix: one if GroupList[j] category dominates GroupList[i] using Student's t-test, otherwise zero. }
#'\item{pValMat[i,j]}{ A p-value of Student's t-test for adjMat[i,j]. }
#'
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
