library(boot)
library(dabestr)
library(simpleboot)
library(ellipsis)
library(igraph)
library(distr)
library(ggplot2)


SimTruncatedDist<-function(nInv,mean,std,p1,p2)
{
  p3<-1-p1-p2
  X<-runif(100, min = 0, max = 99)
  rC1dist<-distr::r(UnivarMixingDistribution(Unif( -400,  400),
                                             Norm(mean=mean, sd=std*2), Cauchy(location=mean+5, scale=2),
                                             mixCoeff=c(p1, p2, p3)) )
  
  V<-c(rC1dist(nInv))
  return(list("V"=V))
}
SimNonNormalDist<-function(nInv,noisePer)
{
  if(missing(nInv)) {
    nInv = 100
    noisePer = 0.05
  }
  if(missing(noisePer)) {
    noisePer = 0.05
  }
  
  initMean=20
  stepMean=60
  std=8
  
  p1<-noisePer
  p2<-0.5
  p3<-1-p1-p2
  
  M1<-initMean + stepMean
  
  V1<-SimTruncatedDist(nInv,M1,std,p1,p2)$V
  
  M1<-initMean+ stepMean
  V2<-SimTruncatedDist(nInv,M1,std,p1,p2)$V
  
  M1<-initMean+ stepMean
  V3<-SimTruncatedDist(nInv,M1,std,p1,p2)$V
  
  M1<-initMean+ stepMean
  V4<-SimTruncatedDist(nInv,M1,std,p1,p2)$V
  
  M1<-initMean+2*stepMean
  V5<-SimTruncatedDist(nInv,M1,std,p1,p2)$V
  
  simData<-c()
  simData$Group<-c(rep(c("C1"),times=nInv),rep(c("C2"),times=nInv),rep(c("C3"),times=nInv),rep(c("C4"),times=nInv),rep(c("C5"),times=nInv) )
  simData$Values <- c(V1,V2,V3,V4,V5)
  return(list("Values"=simData$Values,"Group"=simData$Group,"V1"=V1,"V2"=V2,"V3"=V3,"V4",V4,"V5"=V5))
}
checkSim3Res<-function(adjMat,flag)
{
  TP<-0
  FP<-0
  FN<-0
  TN<-0
  prec<-0
  rec<-0
  F1<-0
  
  if(flag==0)
  {
    f=0
  }
  else
  {
    f=1
  }
  
  for(i in seq(2-f,4-f))
  {
    for(j in seq(1,i-1+f))
    {
      if(adjMat[i,j]==0)
        TN=TN+1
      else
        FP=FP+1
      
    }
  }
  for(j in seq(1,4))
  {
    if(adjMat[5-f,j]==1)
      TP=TP+1
    else
      FN=FN+1
    
  }
  prec<-TP/(TP+FP)
  rec<-TP/(TP+FN)
  F1<- 2*(prec*rec)/(prec+rec)
  
  if(TP+FP ==0)
    prec=0
  if(TP+FN ==0)
    rec=0
  if(prec+rec==0)
    F1=0
  return(list("F1"=F1,"prec"=prec,"rec"=rec))
}


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

# function to obtain R-Squared from the data 
meanBoot <- function(data, indices) {
  d <- data[indices] # allows boot to select sample 
  return(mean(d))
}
diffMeanBoot <- function(Group,g1,g2,Values, indices) {
  d1 <- data$data1[indices] # allows boot to select sample 
  return(mean(d))
}

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
    if(methodType == "bca")
    {
    MegDiffList[[i]] <-
      DataT %>%
      dabest(Group, Values,
             idx = inxList,
             paired = FALSE,
             reps =bootT,
             ci = 100* (1-alpha)
      )
    }
    else #======= NOT USE dabest package
    {
      MegDiffList[[i]] <-  
        bootDiffmeanFunc(DataT$Group,DataT$Values, idx = inxList, reps =bootT, ci = 100* (1-alpha), methodType)
    }
  }
  
  
  return(list("MegDiffList"=MegDiffList))
}

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
#========== result report
printGraph<-function(sortedGroupList,confInvsList,sortedmeanList,adjMat)
{
  N<-dim(confInvsList)[1]
  nameList<-c()
  
  for(i in seq(1,N))
  {
    #nameList[i]<-sprintf("%s: %.0fCI[%f,%f]",sortedGroupList[i],100* (1-alpha),confInvsList[i,1],confInvsList[i,2])
    nameList[i]<-sprintf("%s",sortedGroupList[i])
  }
  g1 <- graph_from_adjacency_matrix( adjMat   ) %>%
    set_vertex_attr("label", value = nameList)
  
  #edge_attr(g1, "label")<-out4$edgeNameList
  
  igraph:: V(g1)$size <- 30*abs(sortedmeanList)/max(sortedmeanList)
  plot(g1, layout =  layout.circle,edge.arrow.size=0.5,vertex.label.color = "black", vertex.label.dist=3)
}

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
  
  value <- list(bootT = bootT, alpha = alpha, Values = Values,Group=Group,adjDiffMat=out4$adjDiffMat, sortedGroupList=out1$sortedGroupList,MegDiffList=out3$MegDiffList, sortedmeanList=out1$sortedmeanList,confInvsList=out2$confInvsList, adjMat=out5$adjMat, adjBootMat=out4$adjMat, methodType=methodType,pValMat=out5$pValMat)
  attr(value, 'class') <- 'EDOIF'
  value
}

print.EDOIF<-function(obj)
{
  cat("EDOIF (Empirical Distribution Ordering Inference Framework) v1.0\n")
  cat("=======================================================\n")
  cat(sprintf("Alpha = %f, Number of bootstrap resamples = %d, CI type = %s\n", obj$alpha,obj$bootT, obj$methodType) )
  N<-dim(obj$confInvsList)[1]
  for(i in seq(1,N))
  {
    cat(sprintf("Distribution: %s\n",obj$sortedGroupList[i]) )
    cat(sprintf("Mean:%f %.0fCI:[ %f,%f]\n",obj$sortedmeanList[i], 100* (1-obj$alpha),obj$confInvsList[i,1],obj$confInvsList[i,2] ) )
  }
  cat("=======================================================\n")
  N<-length(obj$MegDiffList)
  
  
  for(j in seq(1,N))
  {
    if(obj$methodType == "bca")
    {
    CILB<- obj$MegDiffList[[j]]$result$bca_ci_low
    CIUB<- obj$MegDiffList[[j]]$result$bca_ci_high
    }
    else
    {
      CILB<- obj$MegDiffList[[j]]$result$ci_low
      CIUB<- obj$MegDiffList[[j]]$result$ci_high
    }
    ctrlSize<-obj$MegDiffList[[j]]$result$control_size
    testSize<-obj$MegDiffList[[j]]$result$test_size
    meanDiff<-obj$MegDiffList[[j]]$result$difference
    M<-length(CILB)
    currName<-obj$sortedGroupList[j]
    for(i in seq(1,M))
    {
      currDominantName<-obj$sortedGroupList[i+j]
      cat(sprintf("Mean difference of %s (n=%d) minus %s (n=%d)",currDominantName, testSize[i],currName,ctrlSize[i]) )
      
      if(obj$pValMat[i+j,j]>obj$alpha)
        cat(sprintf(": %s \U2280 %s\n :p-val %.4f\n",currName,currDominantName,obj$pValMat[i+j,j]) )
      else
        cat(sprintf(": %s \U227A %s\n :p-val %.4f\n",currName,currDominantName,obj$pValMat[i+j,j]) )
      cat(sprintf("Mean Diff:%f %.0fCI:[ %f,%f]\n\n",meanDiff[i], 100* (1-obj$alpha),CILB[i],CIUB[i] ) )
    }
  }
}

plot.EDOIF<-function(obj,NList,options)
{
  N<-length(obj$MegDiffList)
  if(missing(NList)) {
    NList =  seq(1,N)
  }
  if(missing(options))
  {
    options=0
  }
  #par(mfrow=c(2,1)) 
  
  
  
  for(i in seq(1,length(obj$sortedGroupList) ) )
  {
    obj$sortedGroupList[i] <- sprintf("%d)%s",length(obj$sortedGroupList)-i+1, obj$sortedGroupList[i])
  }
  
  mCILB<-c()
  mCIUB<-c()
  mmeanDiff<-c()
  mXlist<-c()
  mName<-c()
  #============== plot Interval
  for(j in NList)
  {
    if(obj$methodType == "bca")
    {
      CILB<- obj$MegDiffList[[j]]$result$bca_ci_low
      CIUB<- obj$MegDiffList[[j]]$result$bca_ci_high
    }
    else
    {
      CILB<- obj$MegDiffList[[j]]$result$ci_low
      CIUB<- obj$MegDiffList[[j]]$result$ci_high
    }
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
  p1<-ggplot(Data,aes(x=mXlist, y=mmeanDiff,color = mName))  +
    geom_errorbar(aes(ymin=mCILB, ymax=mCIUB), width=.1) +
    geom_line() +
    geom_point() +scale_x_continuous(breaks=2:(N+1), labels = obj$sortedGroupList[2:(N+1)]) +
    theme( axis.text.x = element_text(face="bold", 
                                       size=12, angle=45) )+
    ylab("Mean Differences (B minus A)") +xlab("Target Categories:B")  
  
  p1$labels$colour<-"Based Categories:A"
  if(options ==1 || options ==0)
  {
    plot(p1)
  }
  
  #============== boxplot of CI
  nCILB<-obj$confInvsList[,1]
  nCIUB<-obj$confInvsList[,2]
  nMeanList<-obj$sortedmeanList
  nName<-obj$sortedGroupList
  nXlist<-1:(N+1)
  
  Data2<-data.frame(nMeanList,nCIUB,nCILB,nName, nXlist)
  
  p2<-ggplot(Data2,aes(x=nXlist, y=nMeanList))  +
    geom_errorbar(aes(ymin=nCILB, ymax=nCIUB), width=.1) +
    geom_point() +scale_x_continuous(breaks=1:(N+1), labels = nName) +
    theme( axis.text.x = element_text(face="bold",  
                                      size=12, angle=45) ) +
    
    ylab("Values") +xlab("Categories") 
  if(options ==2 || options ==0)
  {
    plot(p2)
  }
  #==============
  if(obj$methodType == "bca")
    plot(obj$MegDiffList[[1]])
  if(options ==3 || options ==0)
  {
  printGraph(obj$sortedGroupList,obj$confInvsList,obj$sortedmeanList,obj$adjMat)
  }
  
}























