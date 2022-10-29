#' Ploting a spFSR object
#'
#' Plot for a spFSR object. It provides a scatterplot of scoring values vs. iteration. The error bar of scoring values at each iteration can be included. It also allows user to identify the iteration which yields the best scoring value. See \link[spFSR]{spFeatureSelection} for example.
#'
#' @param x a \code{spFSR} object
#' @param errorBar If \code{TRUE}, an error bar of +/- 1 standard deviation will be included around the mean error scoring value at each iteration. When it is \code{TRUE}, the \code{ylim} argument cannot be used. The default is \code{FALSE}.
#' @param annotateBest If \code{TRUE}, the best result will be highlighted and annotated. The default is \code{FALSE}.
#' @param se If \code{TRUE}, an error bar of \eqn{\pm} standard error will be included around the mean error scoring value at each iteration. When it is \code{TRUE}, the \code{ylim} argument cannot be used. The \code{se} does not produce any error bar if \code{errorBar} is set as \code{FALSE}. Note that if the standard error is used, the error bar has a narrower range. The default is \code{FALSE}.
#' @param ... Additional plot parameters that can be passed into the plot function.
#'
#' @return Plot error scoring values vs iterations of a spFSR object with an error bar (if included).
#' @seealso \link[spFSR]{plotImportance} and \link[spFSR]{spFeatureSelection}.
#'
#'
#' @import graphics
#' @export
plot.spFSR <- function(x, errorBar = FALSE,
                        annotateBest = FALSE,
                        se = FALSE, ...){

  if( !inherits(x, 'spFSR')  ){
    stop('Not a spFSR object.')
  }

  stopifnot( is.logical(errorBar) )
  stopifnot( is.logical(annotateBest) )
  stopifnot( is.logical(se) )

  Values <- x$iter.results$values            # extract values
  Iterations <- c( 1:length(Values) )        # create a vector of iterations

  if(	!errorBar ){
    plot(Iterations, Values, ...)            # generic plot
  }else{                                     # add error bar at each iteration
    sdev <- x$iter.results$st.devs
    if(se){
      sdev <-  sdev/sqrt(  length(x$iter.results$importance[[1]])  )
    }
    upper <- Values + sdev
    lower <- Values - sdev
    plot(Iterations, Values, ylim = c( min(lower), max(upper) ), ...)
    arrows(x0 = Iterations, y0 = lower, y1 = upper, code = 0)
    points(lower, pch = '-', cex = 1.5, col = 'red')
    points(upper, pch = '-', cex = 1.5, col = 'red')
  }
  if( annotateBest ){
    if( x$scoring$minimize ){
      v <- which.min(Values)
      h <- min(Values)
    }else{
      v <- which.max(Values)
      h <- max(Values)
    }

    abline( v = v, lty = 'dashed' ) # add vertical line to locate best values
    abline( h = h, lty = 'dashed')        # add horizonal line to locate best values
  }
}
