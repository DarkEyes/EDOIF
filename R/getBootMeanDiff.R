diffMeanBoot <- function(Group,g1,g2,Values, indices) {
  d1 <- data$data1[indices] # allows boot to select sample
  return(mean(d))
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
