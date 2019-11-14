Empirical Distribution Ordering Inference Framework (EDOIF)
===========================================================
[![Travis CI build status](https://travis-ci.com/DarkEyes/EDOIF.svg?branch=master)](https://travis-ci.com/DarkEyes/EDOIF/)[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)[![License](https://img.shields.io/badge/License-BSD%203--Clause-orange.svg)](https://spdx.org/licenses/BSD-3-Clause.html)

Given a dataset of careers and incomes, how large a difference of income between any pair of careers? Given a dataset of travel time records, how long do we need to spend more when choosing a public transportation mode A instead of B to travel? In this work, we developed a framework to solve these problems named "EDOIF".

EDOIF is a non-parametric  framework  based on  Estimation Statistics principle. Its main purpose is to  infer orders of empirical distributions from different categories base on a probability of finding a value in one distribution that greater than the expectation of another distribution. Given a set of ordered-pair of real-category values the framework is capable of 

1) inferring orders of  domination  of  categories  and  representing  orders  in  a form of a graph; 
2) estimating  magnitude  of  difference  between  a  pair  of categories in forms of confidence intervals; and
3) visualizing  domination  orders  and  magnitudes  of  difference of categories.

Installation
------------
Please call the following command in R terminal.

``` r
remotes::install_github("DarkEyes/EDOIF")
```
This requires a user to install the "remotes" package before installing EDOIF.

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
simData1$Values<-c(simData1$Values,rnorm(nInv,mean=initMean,sd=std) )
simData1$Group<-c(simData1$Group,rep(c("Category2"),times=nInv))
simData1$Values<-c(simData1$Values,rnorm(nInv,mean=initMean+2*stepMean,sd=std) )
simData1$Group<-c(simData1$Group,rep(c("Category3"),times=nInv) )
simData1$Values<-c(simData1$Values,rnorm(nInv,mean=initMean+3*stepMean,sd=std) )
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
plot(A1, fontSize=10) # print the results in graphic mode
```
**Graphic mode results**
1. An alpha-confidence-interval of mean plot for five categories. The horizontal axis represents categories and the vertical axis represents values within distributions of categories.

<img src="https://github.com/DarkEyes/EDOIF/blob/master/man/FIG/MeanCI.png" width="550">
2. A dominant-distribution  network  of  five categories. A node represents categories and an edge represents a dominant-distribution relation between categories. If there is an edge from category A to B, then A dominates B. A larger node size implies a higher mean value of a category. 
<img src="https://github.com/DarkEyes/EDOIF/blob/master/man/FIG/DDNet.png" width="450">

3. An alpha-confidence-interval of mean difference plot for five categories.
<img src="https://github.com/DarkEyes/EDOIF/blob/master/man/FIG/MeanDiffCI.png" width="600">

**Text mode results**

```
EDOIF (Empirical Distribution Ordering Inference Framework) v0.1.0
=======================================================
Alpha = 0.050000, Number of bootstrap resamples = 1000, CI type = perc
Using Mann-Whitney test to report whether A ≺ B
A dominant-distribution network density:0.900000
Distribution: Category1
Mean:10.840671 95CI:[ 9.706981,12.014179]
Distribution: Category2
Mean:11.044785 95CI:[ 9.806991,12.446037]
Distribution: Category3
Mean:50.462935 95CI:[ 49.208005,51.757706]
Distribution: Category4
Mean:70.299726 95CI:[ 69.103924,71.502505]
Distribution: Category5
Mean:91.190505 95CI:[ 89.895480,92.518455]
=======================================================
Mean difference of Category2 (n=150) minus Category1 (n=150): Category1 ⊀ Category2
 :p-val 0.4463
Mean Diff:0.204114 95CI:[ -1.545130,1.930609]

Mean difference of Category3 (n=150) minus Category1 (n=150): Category1 ≺ Category3
 :p-val 0.0000
Mean Diff:39.622264 95CI:[ 37.984831,41.378232]

Mean difference of Category4 (n=150) minus Category1 (n=150): Category1 ≺ Category4
 :p-val 0.0000
Mean Diff:59.459055 95CI:[ 57.921328,61.127817]

Mean difference of Category5 (n=150) minus Category1 (n=150): Category1 ≺ Category5
 :p-val 0.0000
Mean Diff:80.349835 95CI:[ 78.620391,82.133270]

Mean difference of Category3 (n=150) minus Category2 (n=150): Category2 ≺ Category3
 :p-val 0.0000
Mean Diff:39.418150 95CI:[ 37.543210,41.241722]

Mean difference of Category4 (n=150) minus Category2 (n=150): Category2 ≺ Category4
 :p-val 0.0000
Mean Diff:59.254941 95CI:[ 57.304359,61.098774]

Mean difference of Category5 (n=150) minus Category2 (n=150): Category2 ≺ Category5
 :p-val 0.0000
Mean Diff:80.145720 95CI:[ 78.313321,82.040234]

Mean difference of Category4 (n=150) minus Category3 (n=150): Category3 ≺ Category4
 :p-val 0.0000
Mean Diff:19.836791 95CI:[ 18.047421,21.762239]

Mean difference of Category5 (n=150) minus Category3 (n=150): Category3 ≺ Category5
 :p-val 0.0000
Mean Diff:40.727570 95CI:[ 39.004372,42.627946]

Mean difference of Category5 (n=150) minus Category4 (n=150): Category4 ≺ Category5
 :p-val 0.0000
Mean Diff:20.890780 95CI:[ 19.079287,22.625807]

```
Contact
=========
- Developer: C. Amornbunchornvej<div itemscope itemtype="https://schema.org/Person"><a itemprop="sameAs" content="https://orcid.org/0000-0003-3131-0370" href="https://orcid.org/0000-0003-3131-0370" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">https://orcid.org/0000-0003-3131-0370</a></div>
- <a href="https://www.nectec.or.th/en/research/dsaru/dsarg-sai.html">Strategic Analytics Networks with Machine Learning and AI (SAI)</a>, <a href="https://www.nectec.or.th/en/">NECTEC</a>, Thailand
- Homepage: <a href="https://sites.google.com/view/amornbunchornvej/home">Link</a>
