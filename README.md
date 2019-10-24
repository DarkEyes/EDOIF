# Empirical Distribution Ordering Inference Framework (EDOIF)

EDOIF is  non-parametric  framework  based on  Esitmation Statistics principle. Its main purpose is to  infer orders of empirical distributions from different categories base on a probability of finding a value in one distribution that greater than the expectation of another distribution. Given a set of ordered-pair of real-category values the framework is capable of 

1) inferring orders of  domination  of  categories  and  representing  orders  in  the form of a graph; 
2) estimating  magnitude  of  difference  between  a  pair  of categories in forms of confidence intervals; and
3) visualizing  domination  orders  and  magnitudes  of  dif-ference of categories.

# Installation
Please call the following commands in R terminal:

library(devtools)

install_github('DarkEyes/EDOIF')

# Examples

library(EDOIF)
#==parameter setting
bootT=1000
alpha=0.05
nInv<-1500

start_time <- Sys.time()
#======= input
simData3<-SimNonNormalDist(nInv=nInv,noisePer=0.01) # generating the simulation data
Values=simData3$Values
Group=simData3$Group
#=============
A3<-EDOIF(Values,Group, bootT=bootT, alpha=alpha, methodType ="perc") # Calling the class constructor
print(A3) # print the results in text mode
plot(A3) # print the results in graphic mode
