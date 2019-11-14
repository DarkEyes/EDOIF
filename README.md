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
EDOIF (Empirical Distribution Ordering Inference Framework) v0.1.0
=======================================================
Alpha = 0.050000, Number of bootstrap resamples = 1000, CI type = perc
Using Mann-Whitney test to report whether A ≺ B
A dominant-distribution network density:0.900000
Distribution: C1
Mean:9.403081 95CI:[ 7.943062,10.886054]
Distribution: C2
Mean:10.793221 95CI:[ 9.322045,12.275161]
Distribution: C3
Mean:49.551156 95CI:[ 48.177049,51.039258]
Distribution: C4
Mean:69.328283 95CI:[ 67.836297,70.803232]
Distribution: C5
Mean:89.713031 95CI:[ 88.133896,91.266592]
=======================================================
Mean difference of C2 (n=100) minus C1 (n=100): C1 ⊀ C2
 :p-val 0.1282
Mean Diff:1.390141 95CI:[ -0.767363,3.359817]

Mean difference of C3 (n=100) minus C1 (n=100): C1 ≺ C3
 :p-val 0.0000
Mean Diff:40.148076 95CI:[ 38.081515,42.079406]

Mean difference of C4 (n=100) minus C1 (n=100): C1 ≺ C4
 :p-val 0.0000
Mean Diff:59.925203 95CI:[ 57.909290,62.081954]

Mean difference of C5 (n=100) minus C1 (n=100): C1 ≺ C5
 :p-val 0.0000
Mean Diff:80.309950 95CI:[ 78.172896,82.411290]

Mean difference of C3 (n=100) minus C2 (n=100): C2 ≺ C3
 :p-val 0.0000
Mean Diff:38.757935 95CI:[ 36.706719,40.771157]

Mean difference of C4 (n=100) minus C2 (n=100): C2 ≺ C4
 :p-val 0.0000
Mean Diff:58.535062 95CI:[ 56.389398,60.730171]

Mean difference of C5 (n=100) minus C2 (n=100): C2 ≺ C5
 :p-val 0.0000
Mean Diff:78.919810 95CI:[ 76.617883,80.863333]

Mean difference of C4 (n=100) minus C3 (n=100): C3 ≺ C4
 :p-val 0.0000
Mean Diff:19.777127 95CI:[ 17.769473,21.850950]

Mean difference of C5 (n=100) minus C3 (n=100): C3 ≺ C5
 :p-val 0.0000
Mean Diff:40.161875 95CI:[ 38.072241,42.040509]

Mean difference of C5 (n=100) minus C4 (n=100): C4 ≺ C5
 :p-val 0.0000
Mean Diff:20.384748 95CI:[ 18.300014,22.516306]

```
Contact
=========
- Developer: C. Amornbunchornvej<div itemscope itemtype="https://schema.org/Person"><a itemprop="sameAs" content="https://orcid.org/0000-0003-3131-0370" href="https://orcid.org/0000-0003-3131-0370" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">https://orcid.org/0000-0003-3131-0370</a></div>
- <a href="https://www.nectec.or.th/en/research/dsaru/dsarg-sai.html">Strategic Analytics Networks with Machine Learning and AI (SAI)</a>, <a href="https://www.nectec.or.th/en/">NECTEC</a>, Thailand
- Homepage: <a href="https://sites.google.com/view/amornbunchornvej/home">Link</a>
