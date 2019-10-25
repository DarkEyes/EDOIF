Empirical Distribution Ordering Inference Framework (EDOIF)
===========================================================

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)


EDOIF is  non-parametric  framework  based on  Esitmation Statistics principle. Its main purpose is to  infer orders of empirical distributions from different categories base on a probability of finding a value in one distribution that greater than the expectation of another distribution. Given a set of ordered-pair of real-category values the framework is capable of 

1) inferring orders of  domination  of  categories  and  representing  orders  in  the form of a graph; 
2) estimating  magnitude  of  difference  between  a  pair  of categories in forms of confidence intervals; and
3) visualizing  domination  orders  and  magnitudes  of  dif-ference of categories.

Installation
------------
Please call the following commands in R terminal.

``` r
library(devtools)
install_github('DarkEyes/EDOIF')
```


Examples
--------
``` r
library(EDOIF)

#== simulation: Generating distributuions of five categories: 
# Category5 dominates Category4
# Category4 dominates Category3
# Category3 dominates Category2
# Category2 dominates Category1

nInv=150 # number of samples per categories
initMean=10
stepMean=20
std=8

simData1<-c()
simData1$Values<-rnorm(nInv,mean=initMean,sd=std)
simData1$Group<-rep(c("Category1"),times=nInv)
simData1$Values<-c(simData1$Values,rnorm(nInv,mean=initMean+5,sd=std) )
simData1$Group<-c(simData1$Group,rep(c("Category2"),times=nInv))
simData1$Values<-c(simData1$Values,rnorm(nInv,mean=initMean+3*stepMean,sd=std) )
simData1$Group<-c(simData1$Group,rep(c("Category3"),times=nInv) )
simData1$Values<-c(simData1$Values,rnorm(nInv,mean=initMean+2*stepMean,sd=std) )
simData1$Group<-c(simData1$Group, rep(c("Category4"),times=nInv) )
simData1$Values<-c(simData1$Values,rnorm(nInv,mean=initMean+4*stepMean,sd=std) )
simData1$Group<-c(simData1$Group, rep(c("Category5"),times=nInv) )

#== parameter setting
bootT=1000 # number of times of sample with replacement in bootstrap function.
alpha=0.05 # Significance level

#== Calling the class constructor
A1<-EDOIF(simData1$Values,simData1$Group, bootT=bootT, alpha=alpha, methodType ="perc") 

#== Visualizing results
print(A1) # print the results in text mode
plot(A1, fontSize=15) # print the results in graphic mode
```
