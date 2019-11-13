#' getMegDiffConfInv function
#'
#' getMegDiffConfInv is a support function for bootstrapping method.
#' Its main purpose is to compute a mean-difference confidence intervals between all pair of distributions.
#'
#'@param Values is a vector of real-number values
#'@param Group is a vector of categories of each real number in Values
#'@param GroupList is a list of names of categories ascendingly ordered by their means.
#'@param bootT is a number of times of sample with replacement for bootstrapping.
#'  The default is 1000. It must be above zero
#'@param alpha is a significance level using in both confidence intervals and ordering inference it has the range [0,1].
#'  The default is 0.05.
#'@param methodType is an option for bootstrapping methods:either  "perc" or "bca".
#'   The "perc" is the default option.
#'@return This function returns a list of mean-difference confidence intervals.
#'
#'\code{MegDiffList} a list of objects that contains mean-difference confidence intervals of all possible pairs of distributions.
#' It contains MegDiffList[[1]],...,MegDiffList[[length(GroupList)]].
#'
#'The \code{MegDiffList} consists of the following variables
#'
#'\item{MegDiffList[[i]]}{ Mean-difference confidence intervals and related information of all categories that have higher means than sortedGroupList[i] category.}
#'
getMegDiffConfInv<-function(Values,Group,GroupList,bootT,alpha,methodType)
{
  if(missing(bootT)) {
    bootT = 1000
  }
  if(missing(alpha)) {
    alpha = 0.05
  }
  N<-length(GroupList)
  DataT<-c()
  DataT$Values<-c(Values)
  DataT$Group<-c(Group)
  DataT<-data.frame(DataT)

  inxList<-c()
  MegDiffList<-list()
  for(i in seq(1,N-1))
  {
    inxList<-c(GroupList[i:N] )
    MegDiffList[[i]] <-
        bootDiffmeanFunc(DataT$Group,DataT$Values, idx = inxList, reps =bootT, ci = 100* (1-alpha), methodType)
  }


  return(list("MegDiffList"=MegDiffList))
}

#' bootDiffmeanFunc function
#'
#' bootDiffmeanFunc is a support function for bootstrapping method.
#' Its main task is to infer mean-difference confidence intervals of distributions for all categories except the first category in idx (idx[2],idx[3],...)  minus a target category (idx[1]).
#'
#'@param Values is a vector of real-number values
#'@param Group is a vector of categories of each real number in Values
#'@param idx is an order list of categories; idx[1] is a target category while others (idx[2],idx[3],...) are compared
#' against idx[1] in order to compute mean-difference confidence intervals.
#'@param reps is a number of time of sampling with replacement in a bootstrapping method.
#'@param ci is a level of confidence interval inferred.
#'@param methodType is a type of method for inferring confidence intervals. It is a parameter of two.boot function of simpleboot package.
#'
#'@return This function returns a list of mean-difference confidence intervals of categories idx[2],idx[3],... minus category idx[1].
#'
#'\code{result} a list of objects that contains mean-difference confidence intervals of pairs of distributions.
#' It contains mean-difference confidence intervals of categories idx[2],idx[3],... minus category idx[1].
#'
bootDiffmeanFunc<-function(Group,Values,idx,reps,ci,methodType)
{
  result<-c()
  N<-length(idx)-1
  result$ci_low<-numeric(N)
  result$ci_high<-numeric(N)
  result$control_size<-numeric(N)
  result$test_size<-numeric(N)
  result$difference<-numeric(N)

  targetGr<-idx[1]
  targerVals<-Values[Group == targetGr]
  for(i in seq(1,N))
  {
    currGr<-idx[i+1]
    currVals<-Values[Group == currGr]
    A<-simpleboot::two.boot(currVals,targerVals,mean,R=reps)
    Aci<-boot::boot.ci(A, conf = ci/100, type = methodType)
    result$ci_low[i]<-Aci[[4]][4]
    result$ci_high[i]<-Aci[[4]][5]
    result$control_size[i]<-length(targerVals)
    result$test_size[i]<-length(currVals)
    result$difference[i]<-mean(currVals)-mean(targerVals)
  }

  return(list("result"=result))
}
