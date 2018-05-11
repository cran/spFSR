#' Summarising an spFSR object
#'
#' @param object A \code{spFSR} object
#' @param ... Additional arguments
#'
#' @return Summary of an spFSR object consisting of number of features selected, wrapper type, total number of iterations, the best performing features, and the descriptive statistics of the best iteration result (the iteration where the best performing features are found).
#'
#' @seealso \link[spFSR]{getImportance}, \link[spFSR]{spFSR.default}, and \link[spFSR]{spFeatureSelection}.
#'
#' @author Yong Kai Wong \email{yongkai1017@gmail.com}
#'
#' @export
summary.spFSR <- function(object, ...){

  if( !inherits(object, 'spFSR')  ){
    warning('Not a spFSR object.')
    importance <- data.frame()
  }else{
    importance <- data.frame(features = object$features,
                         importance = object$importance)
  }

  results <- list( target = object$task.spfs$task.desc$target,
                   importance = importance,
                   nfeatures = object$num.features,
                   niters = object$total.iters,
                   name = object$wrapper$name,
                   best.iter = object$best.iter,
                   best.value = object$best.value,
                   best.std = object$best.std)

  class(results) <- 'summary.spFSR'
  results
}
