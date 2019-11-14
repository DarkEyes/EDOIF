#'
#'
#'
#' print.EDOIF function
#'
#' print.EDOIF is a support function for printing results of ordering inference in text.
#'
#'@param x is an object of EDOIF class that contains the results of ordering inference.
#'
#'@param ... Signature for S3 generic function.
#'
#'@export
#'
print.EDOIF<-function(x, ...)
{
  ellipsis::check_dots_empty()

  # =========== Check object class ===============
  if (class(x)[1] != "EDOIF") {
    stop(paste(
      "An object is not a `EDOIF` class object.")
    )
  } else {
    obj <- x
  }

  cat("EDOIF (Empirical Distribution Ordering Inference Framework) v0.1.0\n")
  cat("=======================================================\n")
  cat(sprintf("Alpha = %f, Number of bootstrap resamples = %d, CI type = %s\n", obj$alpha,obj$bootT, obj$methodType) )
  cat(sprintf("Using Mann-Whitney test to report whether A \U227A B\n"))
  cat(sprintf("A dominant-distribution network density:%f\n",getADJNetDen(obj$adjMat)))
  N<-dim(obj$confInvsList)[1]
  for(i in seq(1,N))
  {
    cat(sprintf("Distribution: %s\n",obj$sortedGroupList[i]) )
    cat(sprintf("Mean:%f %.0fCI:[ %f,%f]\n",obj$sortedmeanList[i], 100* (1-obj$alpha),obj$confInvsList[i,1],obj$confInvsList[i,2] ) )
  }
  cat("=======================================================\n")
  N<-length(obj$MegDiffList)


  for(j in seq(1,N))
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
    ctrlSize<-obj$MegDiffList[[j]]$result$control_size
    testSize<-obj$MegDiffList[[j]]$result$test_size
    meanDiff<-obj$MegDiffList[[j]]$result$difference
    M<-length(CILB)
    currName<-obj$sortedGroupList[j]
    for(i in seq(1,M))
    {
      currDominantName<-obj$sortedGroupList[i+j]
      cat(sprintf("Mean difference of %s (n=%d) minus %s (n=%d)",currDominantName, testSize[i],currName,ctrlSize[i]) )

      if(obj$pValMat[i+j,j]>obj$alpha)
        cat(sprintf(": %s \U2280 %s\n :p-val %.4f\n",currName,currDominantName,obj$pValMat[i+j,j]) )
      else
        cat(sprintf(": %s \U227A %s\n :p-val %.4f\n",currName,currDominantName,obj$pValMat[i+j,j]) )
      cat(sprintf("Mean Diff:%f %.0fCI:[ %f,%f]\n\n",meanDiff[i], 100* (1-obj$alpha),CILB[i],CIUB[i] ) )
    }
  }
}
