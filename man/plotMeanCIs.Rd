% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotGraphphics.R
\name{plotMeanCIs}
\alias{plotMeanCIs}
\title{plotMeanCIs function}
\usage{
plotMeanCIs(obj, fontSize = 15, rankFlag = TRUE)
}
\arguments{
\item{obj}{is an object of EDOIF class that contains the results of ordering inference.}

\item{fontSize}{is a font size of text for all plots.}

\item{rankFlag}{is an option for including ranks of categories with in the plot: default is TRUE for including ranks.}
}
\value{
This function returns a list of an object of ggplot class.

\item{pMeanCI}{ An object of ggplot class containing the plot of mean confidence intervals }
}
\description{
plotMeanCIs is a support function for plotting mean confidence intervals.
}
\examples{
# Generate simulation data with 100 samples per categories

simData<-SimNonNormalDist(nInv=100)

# Performing ordering infernce from simData

resultObj<-EDOIF(simData$Values,simData$Group)

# Get a list of ggplot object of mean confidence intervals

ggplotList<-plotMeanCIs(obj=resultObj)

# Plot mean confidence intervals

plot(ggplotList$pMeanCI)

}
