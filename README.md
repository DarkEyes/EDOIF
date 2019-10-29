Empirical Distribution Ordering Inference Framework (EDOIF)
===========================================================

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)


EDOIF is a non-parametric  framework  based on  Esitmation Statistics principle. Its main purpose is to  infer orders of empirical distributions from different categories base on a probability of finding a value in one distribution that greater than the expectation of another distribution. Given a set of ordered-pair of real-category values the framework is capable of 

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


Example: Inferring orders of categories based on their empirical distributions 
----------------------------------------------------------------------------------
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
**Graphic mode results**
1. An alpha-confidence-interval of mean plot for five categories. The horizontal axis represents categories and the vertical axis represents values within distributions of categories.

<img src="https://github.com/DarkEyes/EDOIF/blob/master/man/FIG/MeanCI.png" width="500">
2. A dominant-distribution  network  of  five categories. A node represents categories and an edge represents a dominant-distribution relation between categories. If there is an edge from category A to B, then A dominates B. A larger node size implies a higher mean value of a category. 
<img src="https://github.com/DarkEyes/EDOIF/blob/master/man/FIG/DDNet.png" width="500">

3. An alpha-confidence-interval of mean difference plot for five categories.
<img src="https://github.com/DarkEyes/EDOIF/blob/master/man/FIG/MeanDiffCI.png" width="600">

**Text mode results**

```
EDOIF (Empirical Distribution Ordering Inference Framework) v1.0
=======================================================
Alpha = 0.050000, Number of bootstrap resamples = 1000, CI type = perc
Distribution: Category1
Mean:10.303180 95CI:[ 9.010756,11.544846]
Distribution: Category2
Mean:14.943284 95CI:[ 13.663984,16.192841]
Distribution: Category4
Mean:49.787612 95CI:[ 48.450130,51.048998]
Distribution: Category3
Mean:69.758789 95CI:[ 68.508522,71.063775]
Distribution: Category5
Mean:90.304677 95CI:[ 88.869291,91.766059]
=======================================================
Mean difference of Category2 (n=150) minus Category1 (n=150): Category1 ≺ Category2
 :p-val 0.0000
Mean Diff:4.640104 95CI:[ 2.781606,6.526985]

Mean difference of Category4 (n=150) minus Category1 (n=150): Category1 ≺ Category4
 :p-val 0.0000
Mean Diff:39.484432 95CI:[ 37.539067,41.208513]

Mean difference of Category3 (n=150) minus Category1 (n=150): Category1 ≺ Category3
 :p-val 0.0000
Mean Diff:59.455610 95CI:[ 57.621261,61.317352]

Mean difference of Category5 (n=150) minus Category1 (n=150): Category1 ≺ Category5
 :p-val 0.0000
Mean Diff:80.001497 95CI:[ 78.041890,81.912140]

Mean difference of Category4 (n=150) minus Category2 (n=150): Category2 ≺ Category4
 :p-val 0.0000
Mean Diff:34.844328 95CI:[ 32.965199,36.628192]

Mean difference of Category3 (n=150) minus Category2 (n=150): Category2 ≺ Category3
 :p-val 0.0000
Mean Diff:54.815505 95CI:[ 52.982562,56.667855]

Mean difference of Category5 (n=150) minus Category2 (n=150): Category2 ≺ Category5
 :p-val 0.0000
Mean Diff:75.361393 95CI:[ 73.553524,77.262215]

Mean difference of Category3 (n=150) minus Category4 (n=150): Category4 ≺ Category3
 :p-val 0.0000
Mean Diff:19.971178 95CI:[ 18.087240,21.732644]

Mean difference of Category5 (n=150) minus Category4 (n=150): Category4 ≺ Category5
 :p-val 0.0000
Mean Diff:40.517065 95CI:[ 38.650440,42.503114]

Mean difference of Category5 (n=150) minus Category3 (n=150): Category3 ≺ Category5
 :p-val 0.0000
Mean Diff:20.545888 95CI:[ 18.774437,22.396629]

```
Contact
=========
- Developer: C. Amornbunchornvej<div itemscope itemtype="https://schema.org/Person"><a itemprop="sameAs" content="https://orcid.org/0000-0003-3131-0370" href="https://orcid.org/0000-0003-3131-0370" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">https://orcid.org/0000-0003-3131-0370</a></div>
- Strategic Analytics Networks with Machine Learning and AI (SAI) https://www.nectec.or.th/en/research/dsaru/dsarg-sai.html
- Homepage: <a href="https://sites.google.com/view/amornbunchornvej/home">Link</a>
