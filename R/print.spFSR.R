print.spFSR <- function(x, ...){
  cat("\nCall:\n")
  print(x$call)

  cat('\nImportance:\n')
  importance <- getImportance(x)
  print( importance )
  cat('\nBest Iteration Results:\n')
  cat( paste('  Iteration Number: ', x$best.iter), '\n')
  cat( paste('  Measure value: ', x$best.value), '\n')
  cat( paste('  Measure standard deviation: ', x$best.std), '\n')

}
