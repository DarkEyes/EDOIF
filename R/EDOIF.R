
#' @title  Empirical Distribution Ordering Inference Framework (EDOIF)
#' @author Chainarong Amornbunchornvej, \email{chai@@ieee.org}
#'
#' @description
#'
#' EDOIF is a non-parametric  framework  based on  Estimation Statistics principle.
#' Its main purpose is to infer orders of empirical distributions from different categories
#' base on a probability of finding a value in one distribution that greater than the expectation
#' of another distribution.
#'
#'  Given a set of ordered-pair of real-category values the framework is capable of
#'  1) inferring orders of  domination  of  categories  and  representing  orders  in  the form of a graph;
#'  2) estimating  magnitude  of  difference  between  a  pair  of categories in forms of confidence intervals; and
#'  3) visualizing  domination  orders  and  magnitudes  of  difference of categories.
#'
#'@param Values is a vector of real-number values
#'@param Group is a vector of categories of each real number in Values
#'@param bootT is a number of times of sample with replacement for bootstrapping.
#'  The default is 1000. It must be above zero
#'@param alpha is a significance level using in both confidence intervals and ordering inference it has the range [0,1].
#'  The default is 0.05.
#'@param methodType is an option for bootstrapping methods:either  "perc" or "bca".
#'   The "perc" is the default option.
#'
#'@return This class constructor returns an object of EDOIF class.
#'
#'\code{obj} an object of EDOIF class that contains the results of ordering inference
#'that can be print in text mode (print(obj)) or graphic mode (plot(obj)).
#'
#'The \code{obj} consists of the following variables
#'
#'\item{Values, Group}{ The main inputs of the framework. They are the double and character vectors respectively. }
#'\item{bootT, alpha, methodType}{ The number of bootstrapping, significance level, and bootstrapping method parameters. }
#'\item{sortedGroupList}{ A list of names of categories ascendingly ordered by their means. }
#'\item{sortedmeanList}{ A list of means of categories that are ascendingly ordered. }
#'\item{MegDiffList[[i]]}{ Mean difference confidence intervals and related information of all categories that have higher means than sortedGroupList[i] category.}
#'\item{confInvsList[i,]}{ A mean confidence interval of sortedGroupList[i] category. confInvsList[i,1] is a lower bound and confInvsList[i,2] is an upper bound. }
#'\item{adjMat[i,j]}{ An element of adjacency matrix: one if sortedGroupList[j] category dominates sortedGroupList[i] using Mann-Whitney test, otherwise zero. }
#'\item{pValMat[i,j]}{ A p-value of Mann-Whitney test for adjMat[i,j]. }
#'\item{adjDiffMat[i,j]}{ A lower bound of confidence interval of mean difference for sortedGroupList[j] minus sortedGroupList[i] using methodType bootstrap. }
#'\item{adjBootMat[i,j]}{ One if adjDiffMat[i,j] is positive, otherwise, zero. }
#'\item{netDen}{ A network density of dominant-distribution network derived from \code{adjMat}.}
#'\item{gObj}{An object of iGraph of a dominant-distribution network. }
#'
#'
#' @examples
#' # Generate simulation data
#'nInv<-100
#'initMean=10
#'stepMean=20
#'std=8
#'simData1<-c()
#'simData1$Values<-rnorm(nInv,mean=initMean,sd=std)
#'simData1$Group<-rep(c("C1"),times=nInv)
#'simData1$Values<-c(simData1$Values,rnorm(nInv,mean=initMean,sd=std) )
#'simData1$Group<-c(simData1$Group,rep(c("C2"),times=nInv))
#'simData1$Values<-c(simData1$Values,rnorm(nInv,mean=initMean+2*stepMean,sd=std) )
#'simData1$Group<-c(simData1$Group,rep(c("C3"),times=nInv) )
#'simData1$Values<-c(simData1$Values,rnorm(nInv,mean=initMean+3*stepMean,sd=std) )
#'simData1$Group<-c(simData1$Group, rep(c("C4"),times=nInv) )
#'simData1$Values<-c(simData1$Values,rnorm(nInv,mean=initMean+4*stepMean,sd=std) )
#'simData1$Group<-c(simData1$Group, rep(c("C5"),times=nInv) )
#'
#' # Performing ordering infernce from simData1
#'
#' resultObj<-EDOIF(simData1$Values,simData1$Group)
#'
#' # Print results in text mode
#'
#' print(resultObj)
#'
#' # Plot results in graphic mode
#'
#' plot(resultObj)
#'
#' @seealso
#'
#' Run \code{vignette("EDOIF_demo", package = "EDOIF")} in a terminal to learn more details about how to use our package.
#'
#'
#'
#'@exportPattern "^[^\\.]"
#'@importFrom boot boot.ci
#'@importFrom boot boot
#'@importFrom simpleboot two.boot
#'@importFrom distr r UnivarMixingDistribution Unif Norm Cauchy
#'@import igraph
#'@import ggplot2
#'@importFrom graphics plot
#'@importFrom stats runif t.test wilcox.test
#'
#'@export

EDOIF <- function(Values,Group,bootT,alpha,methodType) {

  if(missing(bootT)) {
    bootT = 1000
  }
  if(missing(alpha)) {
    alpha = 0.05
  }

  if(missing(methodType))
  {
    methodType="perc"
  }

  out1<-getOrder(Values,Group)
  out2<-getConfInv(Values,Group,out1$sortedGroupList,bootT=bootT,alpha=alpha, methodType = methodType)
  out3<-getMegDiffConfInv(Values,Group,out1$sortedGroupList,bootT=bootT,alpha=alpha, methodType=methodType)
  out4<-getDominantRADJ(out3$MegDiffList,methodType)
  out5<-getWilcoxDominantRADJ(Values,Group,GroupList = out1$sortedGroupList,alpha)
  netDen<-getADJNetDen(out5$adjMat)
  gObj<-getiGraphOBJ(out5$adjMat,out1$sortedGroupList)

  value <- list(bootT = bootT, alpha = alpha, Values = Values,Group=Group,gObj=gObj,netDen=netDen,adjDiffMat=out4$adjDiffMat, sortedGroupList=out1$sortedGroupList,MegDiffList=out3$MegDiffList, sortedmeanList=out1$sortedmeanList,confInvsList=out2$confInvsList, adjMat=out5$adjMat, adjBootMat=out4$adjMat, methodType=methodType,pValMat=out5$pValMat)
  attr(value, 'class') <- 'EDOIF'
  value
}

























