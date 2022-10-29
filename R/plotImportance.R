#' Ploting importance ranks of best performing features from a spFSR object
#'
#' A vertical bar chart of features vs. feature importance. See \link[spFSR]{spFeatureSelection} for example.
#'
#' @param x a \code{spFSR} object
#' @param low Color for the lowest importance. The default is darkblue.
#' @param high Color for the highest importance. The default is black.
#'
#' @return a \code{ggplot} object: a vertical bar chart of features and feature importance.
#'
#' @seealso \link[spFSR]{plotImportance}, \link[spFSR]{spFSR.default}, and \link[spFSR]{spFeatureSelection}.
#'
#' @importFrom ggplot2 ggplot
#' @export
plotImportance <- function(x, low = 'darkblue', high = 'black'){

  if( !inherits(x, 'spFSR')  ){
    stop('Not a spFSR object.')
  }

  importance <- getImportance(x)

  if( !is.null(importance) ){
    x <- reorder(importance$features, importance$importance)
    p <- ggplot2::ggplot(data = importance,
                         ggplot2::aes(x = x,
                                      y = importance,
                                      fill = importance)) +
      ggplot2::scale_fill_gradient(low = low, high = high ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::labs(x = 'Variable',
                    y = 'Importance',
                    title = 'Importance Rank of Variables') +
      ggplot2::ylim( 0, max( importance$importance))

    print(p + ggplot2::coord_flip() )

  }
}
