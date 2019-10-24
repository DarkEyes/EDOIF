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
plot.EDOIF<-function(obj,NList,options, fontSize)
{
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
                                      size=fontSize, angle=45) )+
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
                                      size=fontSize, angle=45) ) +

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
