#' meanBoot function
#'
#' meanBoot is a support function for bootstraping method.
#' Its main purpose is to compute a mean of a given samples from  \code{data} selected by  \code{indices}.
#'
#'@param data is a vector of real-number values
#'@param indices is a vector of TRUE/FALSE indices. It allows boot to select samples.
#'
#'@return This function returns a mean of values in \code{data} that have values TRUE within \code{indices}.

meanBoot <- function(data, indices) {
  d <- data[indices]
  return(mean(d))
}

#' getConfInv function
#'
#' getConfInv is a support function for bootstraping method.
#' Its main purpose is to compute a mean confidence intervals of all distributions.
#'
#'@param Values is a vector of real-number values
#'@param Group is a vector of categories of each real number in Values
#'@param GroupList is a list of names of categories ascendingly ordered by their means.
#'@param bootT is a number of times of sample with replacement for bootstraping.
#'  The default is 1000. It must be above zero
#'@param alpha is a significance level using in both confidence intervals and ordering inference it has the range [0,1].
#'  The default is 0.05.
#'@param methodType is an option for boostrapping methods:either  "perc" or "bca".
#'   The "perc" is the default option.
#'
#'@return This function returns a list of mean confidence intervals.
#'
#'\item{confInvsList[i,]}{ The mean confidence interval of sortedGroupList[i] category. confInvsList[i,1] is a lower bound and confInvsList[i,2] is an upper bound. }
getConfInv<- function(Values,Group,GroupList,bootT,alpha,methodType) {
  if(missing(bootT)) {
    bootT = 1000
  }
  if(missing(alpha)) {
    alpha = 0.05
  }
  K<-length(GroupList)
  confInvsList<-matrix(0,K,2)
  for(k in seq(1,K))
  {
    bRes<- boot(data=Values[Group==GroupList[k]], statistic=meanBoot,
                R=bootT)
    confInvsList[k,]<-boot.ci(bRes, type=methodType,conf = 1-alpha)[[4]][c(4,5)]
  }
  return(list("confInvsList"=confInvsList))
}
