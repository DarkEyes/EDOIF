
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
