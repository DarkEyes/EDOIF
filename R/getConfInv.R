meanBoot <- function(data, indices) {
  d <- data[indices] # allows boot to select sample
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
