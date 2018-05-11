#' Extract feature importance data from a spFSR object
#'
#' @description Returns importance ranks of best performing features.
#' See \link[spFSR]{spFeatureSelection} for a more detailed example.
#'
#' @param x a \code{spFSR} object
#' @return A \code{data.frame} of features and feature importance
#'
#' @seealso \link[spFSR]{plotImportance} and \link[spFSR]{spFeatureSelection}.
#'
#' @author Yong Kai Wong \email{yongkai1017@gmail.com}
#'
#' @export
getImportance <- function(x){
  if( !inherits(x, 'spFSR')  ){
    warning('Not a spFSR object.')
    importance <- NULL
  }else{
    importance <- data.frame(features = x$features,
                             importance = x$importance)
  }
  return(importance)
}
