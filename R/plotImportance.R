#' Plot importance ranks of best performing features from a spFSR object
#'
#' @description Return a vertical bar chart of features vs. feature importance.
#' See \link[spFSR]{spFeatureSelection} for a more detailed example.
#'
#' @param x an \code{spFSR} object
#' @param low Color for the lowest importance. The default is darkblue.
#' @param high Color for the highest importance. The default is black.
#'
#' @return a \code{ggplot} object: a vertical bar chart of features and feature importance.
#'
#' @seealso \link[spFSR]{plotImportance}, \link[spFSR]{spFSR.default}, and \link[spFSR]{spFeatureSelection}.
#'
#' @author Yong Kai Wong \email{yongkai1017@gmail.com}
#' @importFrom ggplot2 ggplot
#' @export
plotImportance <- function(x, low = 'darkblue', high = 'black'){

  if( !inherits(x, 'spFSR')  ){
    stop('Not an spFSR object.')
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
      ggplot2::ylim( 0, max( 0.5, max( importance$importance  ) ))

    print(p + ggplot2::coord_flip() )

  }
}
