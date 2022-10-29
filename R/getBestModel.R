#' Extracting the wrapped model of the best performing features from a spFSR object
#'
#' A fitted model uses the best performing feature subsets. It inherits all methods or functions applied to a \code{WrappedModel} objects. For example, the \code{predict} function can be used on the fitted model. See \link[spFSR]{spFeatureSelection} for example.
#'
#' @param x a \code{spFSR} object
#' @return A \code{WrappedModel} object of the best performing features.

#'
#' @seealso \link[spFSR]{spFeatureSelection}
#'
#'
#' @export
getBestModel <- function(x){
  if( !inherits(x, 'spFSR')  ){
    stop('Not a spFSR object.')
  }
  x$best.model
}
