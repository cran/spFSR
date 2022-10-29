#' Extracting feature importance data from a spFSR object
#'
#' This returns importance ranks of best performing features. See \link[spFSR]{spFeatureSelection} for example.
#'
#' @param x a \code{spFSR} object
#' @return A \code{data.frame} of features and feature importance
#'
#' @seealso \link[spFSR]{plotImportance} and \link[spFSR]{spFeatureSelection}.
#'
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
