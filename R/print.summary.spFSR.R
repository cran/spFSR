print.summary.spFSR <- function(x, ...){

  cat( '\nSummary of SPSA for feature selection')
  cat( '\nTarget variable:', x$target )
  cat( '\nNumber of features: ', x$nfeatures, '\n')
  cat( '\nWrapper: ', x$name )
  cat( '\nTotal number of iterations: ', x$niters)
  cat( '\nBest Features \n')
  print( x$importance )
  cat('\nBest Iteration Results\n')
  cat( paste('  Iteration Number: ', x$best.iter), '\n')
  cat( paste('  Measure value: ', format(x$best.value), ...) , '\n')
  cat( paste('  Measure standard deviation: ', format(x$best.std), ...), '\n')
}
