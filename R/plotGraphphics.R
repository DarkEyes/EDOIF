#' plotGraph function
#'
#' plotGraph is a support function for plotting a dominant-distribution network from an adjacency matrix.
#'
#' @param obj is an object of EDOIF class that contains the results of ordering inference.
#' @param rankFlag is an option for including ranks of categories with in the plot: default is TRUE for including ranks.
#'
#' @return This function returns an object of iGraph for a dominant-distribution network and its plot variable.
#'
#'\item{graphVar}{ An object of iGraph for a dominant-distribution network }
#'
plotGraph<-function(obj,rankFlag)
{
  if(missing(rankFlag))
  {
    rankFlag=TRUE
  }

  sortedGroupList<-obj$sortedGroupList
  confInvsList<-obj$confInvsList
  sortedmeanList<-obj$sortedmeanList
  adjMat<-obj$adjMat

  N<-dim(confInvsList)[1]
  nameList<-c()

  if(rankFlag == TRUE)
  {
    # Add ranks within an order to category labels.
    for(i in seq(1,length(sortedGroupList) ) )
    {
      sortedGroupList[i] <- sprintf("%d)%s",length(sortedGroupList)-i+1, sortedGroupList[i])
    }
  }

  g1 <- graph_from_adjacency_matrix( adjMat   ) %>%
    set_vertex_attr("label", value = sortedGroupList)

  igraph:: V(g1)$size <- 30*abs(sortedmeanList)/max(sortedmeanList)
  plot(g1, layout =  layout.circle,vertex.label.color = "black", vertex.label.dist=3)
  return(list("graphVar"=g1) )
}

#' plotMeanDiffCIs function
#'
#' plotMeanDiffCIs is a support function for plotting difference-mean confidence intervals.
#'
#' @param obj is an object of EDOIF class that contains the results of ordering inference.
#' @param NList is a list of based categories users want to have in mean-difference CI plot.
#' @param rankFlag is an option for including ranks of categories with in the plot: default is TRUE for including ranks.
#' @param fontSize is a font size of text for all plots.
#'
#' @return This function returns an object of ggplot class.
#'
#'\item{pDiffCI}{ An object of ggplot class containing the plot of difference-mean confidence intervals }
#'
plotMeanDiffCIs<-function(obj,NList,fontSize,rankFlag)
{
  if(missing(fontSize))
  {
    fontSize=15
  }
  if(missing(rankFlag))
  {
    rankFlag=TRUE
  }
  N<-length(obj$MegDiffList)
  if(missing(NList)) {
    NList =  seq(1,N)
  }

  if(rankFlag == TRUE)
  {
    for(i in seq(1,length(obj$sortedGroupList) ) ) # Add ranks within an order to category labels.
    {
      obj$sortedGroupList[i] <- sprintf("%d)%s",length(obj$sortedGroupList)-i+1, obj$sortedGroupList[i])
    }
  }

  mCILB<-c()
  mCIUB<-c()
  mmeanDiff<-c()
  mXlist<-c()
  mName<-c()


  for(j in NList)
  {

    CILB<- obj$MegDiffList[[j]]$result$ci_low
    CIUB<- obj$MegDiffList[[j]]$result$ci_high

    m<-length(CILB)
    meanDiff<-obj$MegDiffList[[j]]$result$difference
    mXlist<-c(mXlist,j:N+1)
    mCILB<-c(mCILB,CILB)
    mCIUB<-c(mCIUB,CIUB)
    mmeanDiff<-c(mmeanDiff,meanDiff)

    mName<-c(mName,rep(obj$sortedGroupList[j],times=m))
    #plotCI(1:13, meanDiff, ui=CIUB, li=CILB )
  }
  Data<-data.frame(mXlist,mCILB,mCIUB,mmeanDiff,mName)
  # Standard error of the mean
  pDiffCI<-ggplot(Data,aes(x=mXlist, y=mmeanDiff,color = mName))  +
    geom_errorbar(aes(ymin=mCILB, ymax=mCIUB), width=.1) +
    geom_line() +
    geom_point() +scale_x_continuous(breaks=2:(N+1), labels = obj$sortedGroupList[2:(N+1)]) +
    theme( axis.text.x = element_text(face="bold",
                                      size=fontSize, angle=45) )+
    ylab("Mean Differences (B minus A)") +xlab("Target Categories:B")

  pDiffCI$labels$colour<-"Based Categories:A"
  return(list("pDiffCI"=pDiffCI) )
}

#' plotMeanCIs function
#'
#' plotMeanCIs is a support function for plotting mean confidence intervals.
#'
#' @param obj is an object of EDOIF class that contains the results of ordering inference.
#' @param rankFlag is an option for including ranks of categories with in the plot: default is TRUE for including ranks.
#' @param fontSize is a font size of text for all plots.
#'
#' @return This function returns an object of ggplot class.
#'
#'\item{pMeanCI}{ An object of ggplot class containing the plot of mean confidence intervals }
#'
plotMeanCIs<-function(obj,fontSize,rankFlag)
{
  if(missing(fontSize))
  {
    fontSize=15
  }
  if(missing(rankFlag))
  {
    rankFlag=TRUE
  }

  if(rankFlag == TRUE)
  {
    for(i in seq(1,length(obj$sortedGroupList) ) ) # Add ranks within an order to category labels.
    {
      obj$sortedGroupList[i] <- sprintf("%d)%s",length(obj$sortedGroupList)-i+1, obj$sortedGroupList[i])
    }
  }
  #============== plot of mean CI
  N<-length(obj$MegDiffList)
  nCILB<-obj$confInvsList[,1]
  nCIUB<-obj$confInvsList[,2]
  nMeanList<-obj$sortedmeanList
  nName<-obj$sortedGroupList
  nXlist<-1:(N+1)

  Data2<-data.frame(nMeanList,nCIUB,nCILB,nName, nXlist)

  pMeanCI<-ggplot(Data2,aes(x=nXlist, y=nMeanList))  +
    geom_errorbar(aes(ymin=nCILB, ymax=nCIUB), width=.1) +
    geom_point() +scale_x_continuous(breaks=1:(N+1), labels = nName) +
    theme( axis.text.x = element_text(face="bold",
                                      size=fontSize, angle=45) ) +

    ylab("Values") +xlab("Categories")
  return(list("pMeanCI"=pMeanCI))
}

#' plot.EDOIF function
#'
#' plot.EDOIF is a support function for printing all plots of EDOIF framework:
#'  dominant-distribution network plot, mean CI plot, and mean-difference CI plot.
#'
#'@param x is an object of EDOIF class that contains the results of ordering inference.
#'@param NList is a list of based categories users want to have in mean-difference CI plot.
#'@param options is an option of reporting EDOIF plot(s):
#' 0 for reporting all plots, 1 for mean-difference CI plot, 2 for mean CI plot,
#'  and 3 for dominant-distribution network plot.
#'
#'@param ... Signature for S3 generic function.
#'
#'@param fontSize is a font size of text for all plots.
#'
#'@export
plot.EDOIF<-function(x, ... ,NList,options, fontSize)
{
  ellipsis::check_dots_empty()

# =========== Check object class ===============
  if (class(x)[1] != "EDOIF") {
    stop(paste(
      "An object is not a `EDOIF` class object.")
    )
  } else {
    obj <- x
  }


  N<-length(obj$MegDiffList)
  if(missing(NList)) {
    NList =  seq(1,N)
  }
  if(missing(options))
  {
    options=0
  }
  if(missing(fontSize))
  {
    fontSize=15
  }
  #par(mfrow=c(2,1))

  theme_update(text = element_text(size=fontSize)  )

  if(options ==1 || options ==0)
  {
    p1<-plotMeanDiffCIs(obj,NList,fontSize)$pDiffCI
    plot(p1)
  }


  if(options ==2 || options ==0)
  {
    p2<-plotMeanCIs(obj)$pMeanCI
    plot(p2)
  }

  if(options ==3 || options ==0)
  {
    graphVar<-plotGraph(obj)
  }
}
