#' Default function of feature selection and ranking by SP-FSR
#'
#' @description This is the default function of \link[spFSR]{spFeatureSelection}.
#' See \link[spFSR]{spFeatureSelection} for more details.
#'
#' @param task A \code{task} object created using \pkg{mlr} package. It must be either a \code{ClassifTask} or \code{RegrTask} object.
#' @param wrapper A \code{Learner} object created using \pkg{mlr} package. Multiple learners object is not supported.
#' @param measure A performance measure within the \pkg{mlr} package supported by the \code{task}.
#' @param norm.method Normalization method for features. NULL value is allowed. Supported methods are 'standardize', 'range', 'center', and 'scale'. Default value is 'standardize'.
#' @param num.features.selected Number of features selected. It must be a nonnegative integer and must not exceed the total number of features in the task. A value of 0 results in automatic feature selection. Default value is 0L.
#' @param features.to.keep Names of features to keep in addition to \code{num.features.selected}. Default value is NULL.
#' @param iters.max Maximum number of iterations to execute. The minimum value is 2L. Default value is 100L.
#' @param stall.limit Number of iterations to stall, that is, to continue without at least \code{stall.tolerance} improvement to the measure value. The mininum value is 2L. Default value is 20L.
#' @param stall.tolerance Value of stall tolerance. It must be strictly positive. Default value is 1/10^7.
#' @param num.grad.avg Number of gradients to average for gradient approximation. It must be a positive integer. Default value is 3L.
#' @param num.gain.smoothing Number of most recent gains to use in gain smoothing. It must be a positive integer. Default value is 3L.
#' @param perturb.amount Perturbation amount for feature importances during gradient approximation. It must be a value between 0.01 and 0.1. Default value is 0.05.
#' @param gain.min The minimum gain value. It must be greater than or equal to 0.001. Default value is 0.01.
#' @param gain.max The maximum gain value. It must be greater than or equal to \code{gain.min}.  Default value is 1.0.
#' @param perf.eval.method Performance evaluation method. It must be either 'cv' for cross-validation or 'resub' for resubstitution. Default is 'cv'.
#' @param num.cv.folds The number of cross-validation folds when 'cv' is selected as \code{perf.eval.method}. The minimum value is 3L. Default value is 5L.
#' @param num.cv.reps.grad.avg The number of cross-validation repetitions for gradient averaging. It must be a positive integer. Default value is 3L.
#' @param num.cv.reps.feat.eval The number of cross-validation repetitions for feature subset evaluation. It must be a positive integer. Default value is 3L.
#' @param cv.stratify Logical argument. Stratify cross-validation? Default value is TRUE.
#' @param run.parallel Logical argument. Perform cross-validations in parallel? Default value is TRUE.
#' @param num.cores Number of cores to use in case of a parallel run. It must be less than or equal to the total number of cores on the host machine. If set to \code{NULL} when \code{run.parallel} is \code{TRUE}, it is taken as one less of the total number of cores.
#' @param show.info If set to \code{TRUE}, iteration information is displayed at print frequency.
#' @param print.freq Iteration information printing frequency. It must be a positive integer. Default value is 1L.
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
#' \item{run.time}{Total run time in minutes.}
#' \item{call}{Call.}
#'
#' @seealso \link[spFSR]{spFeatureSelection}.
#' @author Vural Aksakalli \email{vaksakalli@gmail.com}
#' @author Babak Abbasi \email{babak.abbasi@rmit.edu.au}, \email{b.abbasi@gmail.com}
#' @author Yong Kai Wong \email{yongkai.wong@rmit.edu.au}, \email{yongkai1017@gmail.com}
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
#' @export
spFSR.default <- function(
  task,
  wrapper,
  measure,
  norm.method = 'standardize',
  num.features.selected = 0L,
  features.to.keep = NULL,
  iters.max = 100L,
  stall.limit = 20L,
  stall.tolerance = 10^(-7),
  num.grad.avg = 3L,
  num.gain.smoothing = 3L,
  perturb.amount = 0.05,
  gain.min = 0.01,
  gain.max = 1.0,
  perf.eval.method = 'cv',
  num.cv.folds = 5L,
  num.cv.reps.grad.avg  = 3L,
  num.cv.reps.feat.eval = 3L,
  cv.stratify = TRUE,
  run.parallel = TRUE,
  num.cores = NULL,
  show.info = TRUE,
  print.freq = 1L
){

	model <- spsaKernel(
				  task = task,
				  wrapper = wrapper,
				  measure = measure,
				  norm.method = norm.method,
				  num.features.selected = num.features.selected,
				  features.to.keep = features.to.keep,
				  iters.max = iters.max,
				  stall.limit = stall.limit,
				  stall.tolerance = stall.tolerance,
				  num.grad.avg = num.grad.avg,
				  num.gain.smoothing = num.gain.smoothing,
				  perturb.amount = perturb.amount,
				  gain.min = gain.min,
				  gain.max = gain.max ,
				  perf.eval.method = perf.eval.method,
				  num.cv.folds = num.cv.folds,
				  num.cv.reps.grad.avg  = num.cv.reps.grad.avg,
				  num.cv.reps.feat.eval = num.cv.reps.feat.eval,
				  cv.stratify = cv.stratify,
				  run.parallel = run.parallel,
				  num.cores = num.cores,
				  show.info = show.info,
				  print.freq = print.freq)
	model$call <- match.call()

	class(model) <- 'spFSR'
	return(model)
}
