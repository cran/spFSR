#' Feature selection and ranking by SPSA-FSR
#'
#' @description Searches for the best performing set of features,
#' either automatically or for a given number of features, and ranks them by their
#' importance via the simultaneous perturbation stochastic approximation (SPSA) algorithm for
#' given a task, wrapper, and performance criterion. The task, the wrapper, and
#' the performance criterion are defined using the \pkg{mlr} package.
#'
#' @param task A \code{task} object created using \pkg{mlr} package. It must be either a \code{ClassifTask} or \code{RegrTask} object.
#' @param wrapper A \code{Learner} object created using \pkg{mlr} package. Multiple learners object is not supported.
#' @param measure A performance measure supported by the \code{task}.
#' @param norm.method Normalization method for features. NULL value is allowed. Supported methods are 'standardize', 'range', 'center', and 'scale'. Default value is 'standardize'.
#' @param num.features.selected Number of features to be selected. Must be between zero and total number of features in the task. A value of zero results in automatic feature selection.
#' @param ... Additional arguments. For more details, see \link[spFSR]{spFSR.default}.
#'
#' @examples
#' \donttest{
#' data(iris)         # load the data
#' library(mlr)       # load the mlr package
#'
#' if( requireNamespace('class', quietly = TRUE) ){
#'
#'   # Load class so that a knn classifier can be defined
#'   library('class')
#'
#'   # define classification task on 20 random samples
#'   task    <- makeClassifTask(data = iris[sample(150, 20),],
#'                              target = 'Species')
#'
#'   # define a wrapper (1-KNN classifier)
#'   wrapper <- makeLearner('classif.knn', k = 1)
#'
#'
#'   # run spsa with 2 iterations
#'   # to select 1 out of 4 features
#'   spsaMod <- spFeatureSelection( task = task,
#'                                  wrapper = wrapper,
#'                                  measure = mmce,
#'                                  num.features.selected = 1,
#'                                  num.cores = 1,
#'                                  iters.max = 2)
#'
#'   # obtain summary
#'   summary(spsaMod)
#'
#'
#'   # plot with error bars
#'   plot(spsaMod, errorBar = TRUE)
#'
#'   # obtain the wrapped model with the best performing features
#'   bestMod <- getBestModel(spsaMod)
#'
#'   # predict using the best mod
#'   pred    <- predict(bestMod, task = spsaMod$task.spfs )
#'
#'   # Obtain confusion matrix
#'   calculateConfusionMatrix( pred )
#'
#'   # Get the importance ranks of best performing features
#'   getImportance(spsaMod)
#'   plotImportance(spsaMod)
#'
#'   }
#'
#' }
#'
#'
#' @return \code{spFSR} returns an object of class "spFSR". An object of class "spFSR" consists of the following:
#'
#' \item{task.spfs}{An \pkg{mlr} package \code{task} object defined on the best performing features.}
#' \item{wrapper}{An \pkg{mlr} package \code{learner} object as specified by the user.}
#' \item{measure}{An \pkg{mlr} package performance measure as specified by the user.}
#' \item{param best.model}{An \pkg{mlr} package \code{WrappedModel} object trained by the \code{wrapper} using \code{task.spfs}.}
#' \item{iter.results}{A \code{data.frame} object containing detailed information on each iteration.}
#' \item{features}{Names of the best performing features.}
#' \item{num.features}{The number of best performing features.}
#' \item{importance}{A vector of importance ranks of the best performing features.}
#' \item{total.iters}{The total number of iterations executed.}
#' \item{best.iter}{The iteration where the best performing feature subset was encountered.}
#' \item{best.value}{The best measure value encountered during execution.}
#' \item{best.std}{The standard deviation corresponding to the best measure value encountered.}
#' \item{run.time}{Total run time in minutes}
#' \item{rdesc.feat.eval}{Resampling specification}
#' \item{call}{Call}
#'
#' @author Vural Aksakalli \email{vaksakalli@gmail.com}
#' @author Babak Abbasi \email{babak.abbasi@rmit.edu.au}, \email{b.abbasi@gmail.com}
#' @author Yong Kai Wong \email{yongkai1017@gmail.com}
#'
#' @references V. Aksakalli and M. Malekipirbazari (2015) Feature Selection via Binary Simultaneous Perturbation Stochastic Approximation,  \emph{Pattern Recognition Letters}, \bold{Vol. 75}, 41 -- 47. See \url{https://doi.org/10.1016/j.patrec.2016.03.002}
#'
#' @import parallelMap
#' @import stats
#' @import parallel
#' @import tictoc
#' @import mlr
#'
#' @importFrom mlr getTaskFeatureNames
#'
#' @rdname spFeatureSelection
#'
#' @seealso \link[mlr]{makeClassifTask}, \link[mlr]{makeRegrTask}, \link[mlr]{makeLearner} and \link[spFSR]{spFSR.default}.
#' @export
spFeatureSelection <- function(
  task,
  wrapper,
  measure,
  norm.method = 'standardize',
  num.features.selected,
  ...
){

  model <- spFSR.default(
    task = task,
    wrapper = wrapper,
    measure = measure,
    norm.method = norm.method,
    num.features.selected = num.features.selected,
    ...
  )
  model
}
