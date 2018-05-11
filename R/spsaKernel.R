spsaKernel <- function(
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
) {

  # Note: sections and comments will be deleted
  # 0. ON EXIT ----
  # Switch back to default settings in configureMlr and stop parallel
  # regardless if the spfs results in error or not.

  on.exit(
    { mlr::configureMlr(show.learner.output = TRUE, show.info = TRUE)
      parallelMap::parallelStop()
    }
  )


  # 1. Check the versions of R and imported packages (PASSED) ----
  # Note: no need to check these which can be done in DESCRIPTION FILE.

  # 2. ARGUMENT CHECKS ----
  # make sure task, wrapper, and measure are defined properly

  if( !inherits(measure, 'Measure') ){
    stop('Not a supported performance measure. See mlr package for more details.')
  }else{


    if(  !any( measure %in%  mlr::listMeasures(task) )   ){
      stop( paste(measure$id, 'measure is not supported by', task$task.desc$type ,'task'))
    }
  }

  if( !inherits(wrapper, "RLearnerClassif") && cv.stratify ){
    warning(paste0('\nStratification for learner of type ',wrapper$type,' is not supported.'))
    warning('\ncv.stratify is reset to FALSE.\n')
    cv.stratify <- FALSE
  }

  if(!is.null(norm.method)) {
    ## we will let mlr handle invalid values
    stopifnot(is.character(norm.method))
  }

  stopifnot(!is.null(perf.eval.method), perf.eval.method %in% c('cv', 'resub'))

  stopifnot(is.logical(cv.stratify))
  stopifnot(is.logical(run.parallel))
  stopifnot(is.logical(show.info))

  if (run.parallel & !is.null(num.cores)) {
    stopifnot(is.numeric(num.cores),
              num.cores %% 1 == 0,
              num.cores >= 1)
  }

  if(run.parallel) {
    num.cores.max <- parallel::detectCores()
    if(is.null(num.cores)) {
      num.cores <- num.cores.max - 1
    } else {
      if(num.cores > num.cores.max){
        warning(paste0('Warning: Max number of cores is reset to the number of available cores = ', num.cores.max, '.'))
        num.cores <- num.cores.max
      }
    }
    parallelMap::parallelStop()
    parallelMap::parallelStartSocket(num.cores, show.info = FALSE)
  }

  if (!is.null(features.to.keep)) {
    stopifnot(is.character(features.to.keep))
  }

  stopifnot(is.numeric(iters.max),
            iters.max %% 1 == 0,
            iters.max >= 2)

  stopifnot(is.numeric(stall.limit),
            stall.limit %% 1 == 0,
            stall.limit >= 2)

  stopifnot(is.numeric(stall.tolerance),
            stall.tolerance >= 10^(-10))

  stopifnot(is.numeric(num.grad.avg),
            num.grad.avg %% 1 == 0,
            num.grad.avg >= 1)

  stopifnot(is.numeric(perturb.amount),
            perturb.amount >= 0.01,
            perturb.amount <= 0.1)

  if (perturb.amount != 0.05){
		warning('Perturbation amount other than 0.05 is not recommended!')
	}

  stopifnot(is.numeric(gain.max),
            gain.max >= 0.001)

  stopifnot(is.numeric(gain.min),
            gain.min >= 0.001,
            gain.min <= gain.max)

  stopifnot(is.numeric(num.cv.folds),
            num.cv.folds %% 1 == 0,
            num.cv.folds >= 3)

  if (show.info) {
    stopifnot(is.numeric(print.freq),
              print.freq %% 1 == 0,
              print.freq >= 1)
  }

  stopifnot(is.numeric(num.cv.reps.grad.avg),
            num.cv.reps.grad.avg %% 1 == 0,
            num.cv.reps.grad.avg >= 1)

  stopifnot(is.numeric(num.cv.reps.feat.eval),
            num.cv.reps.feat.eval %% 1 == 0,
            num.cv.reps.feat.eval >= 1)

  stopifnot(is.numeric(num.gain.smoothing),
            num.gain.smoothing %% 1 == 0,
            num.gain.smoothing >= 1)

  p <- sum( task$task.desc$n.feat ) # extract the number of features

  stopifnot(is.numeric(num.features.selected),
            num.features.selected %% 1 == 0,
            num.features.selected >= 0,
            num.features.selected <= p)

  # 3. NORMALIZATION, RESAMPLING, and MLR OPTION CONFIGURATION ----
  # Normalize features if norm.method is specified

  if(!is.null(norm.method)) {
    task <- mlr::normalizeFeatures(task, method = norm.method)
  }

  if (num.cv.reps.grad.avg == 1) {
    rdesc.grad.avg <- mlr::makeResampleDesc("CV", iters = num.cv.folds,
                                            stratify = cv.stratify)
  } else {
    rdesc.grad.avg <- mlr::makeResampleDesc("RepCV", folds = num.cv.folds,
                                            reps = num.cv.reps.grad.avg,
                                            stratify = cv.stratify)
  }

  if (num.cv.reps.feat.eval == 1) {
    rdesc.feat.eval <- mlr::makeResampleDesc("CV", iters = num.cv.folds,
                                             stratify = cv.stratify)
  } else {
    rdesc.feat.eval <- mlr::makeResampleDesc("RepCV", folds = num.cv.folds,
                                             reps = num.cv.reps.feat.eval,
                                             stratify = cv.stratify)
  }

  mlr::configureMlr(show.learner.output = FALSE, show.info = FALSE)

	# 4. SELECT FEATURES FUNCTION ----
  # given the imp. vector,
  # this function determines which features to select (as indices)
  select.features <- function(imp) {
    imp[features.to.keep.idx] <- 1
    if (num.features.selected == 0) {
      num.features.to.select <- sum(imp >= 0.5)
      if (num.features.to.select == 0) {
        num.features.to.select <- 1 ## select at least one!
      }
    } else {
      num.features.to.select <- min(length(imp), length(features.to.keep.idx) + num.features.selected)
    }
    order(imp, decreasing = TRUE)[1:num.features.to.select]
  }

  ################################
  # this function performs either repeated f-fold CV or computes the resubstitution performance
  fs.perf <- function(rdesc, curr.imp) {
    selected.features <- select.features(curr.imp)
    task.fs <- mlr::dropFeatures(task, features = getTaskFeatureNames(task)[-selected.features])

    if (perf.eval.method == "cv") {
      result <- mlr::resample(learner = wrapper, task = task.fs,
                              resampling = rdesc, measures = measure, keep.pred = FALSE)
      best.value.mean <- mean(result$measures.test[[2]])
      best.value.std  <- sd(result$measures.test[[2]])
    } else if (perf.eval.method == "resub") {
      # train the learner using the task
      model.task <- mlr::train(learner = wrapper, task = task.fs)
      # get predictions for the task
      pred.task <- predict(model.task, newdata = mlr::getTaskData(task))
      # evaluate the prediction performance
      best.value.mean <- mlr::performance(pred = pred.task, measures = measure)
      best.value.std  <- 0.0
    } else {
      stop(paste0("Unsupported performance evaluation method: ", perf.eval.method))
    }

    list(opt.sign * best.value.mean, best.value.std)
  } # end fs.perf

  # this function accepts "feature names" to keep
  # here we convert these names to indices, because this is what we need
  # if the names contain any features that do not exist, execution will stop
  # NULL and empty string are OK for features.to.keep
  # because they both return length(features.to.keep.idx) = 0
  features.to.keep.idx <- sapply(features.to.keep, function(x, list)
                             which(unlist(lapply(list, function(y, z) z %in% y, z=x))),
                             list = getTaskFeatureNames(task))
  features.to.keep.idx <- sort(as.numeric(features.to.keep.idx))

  if (length(features.to.keep.idx) < length(features.to.keep)) {
    stop("Invalid feature name(s) to keep.")
  }

  # we need the opt. sign to correctly multiply the best performance values
  opt.sign <- 1 # +1 for minimization, -1 for maximization
  if (measure$minimize == FALSE) {opt.sign <- -1}

  ##############################################
  ### initializations:
  stall.counter <- 1
  best.value <- opt.sign * Inf
  best.std <- Inf
  imp.min  <- rep(0.0, p) ## lower bound for spfs importance
  imp.max  <- rep(1.0, p) ## upper bound for spfs importance
  same.features.counter.max <- round(gain.max / gain.min) ## initialize to the ratio of max to min gain
  same.features.counter.max <- max(10, min(100, same.features.counter.max)) ## limit between 10 and 100
  ##############################################

  run.time <- -1
  tictoc::tic()

  curr.imp <- rep(0.5, p) ## initial importances

  if (show.info ) {
    message('SPSA-FSR begins:')
    message('Wrapper = ' , wrapper$short.name)
    message('Measure = ' , measure$id)
    message('Number of selected features = ',num.features.selected)
    cat('\niter ', 'value', '  st.dev', ' num.ft ', 'best.value\n')
  }

  # iter.results stores results per spfs iteration
  iter.results <- list(values = numeric(0),
                       stds = numeric(0),
                       gain.sequence = numeric(0),
                       importances = list(),
                       features = list())

  ##############################################
  ### initializations:
  ghat <- rep(0.0, p)
  selected.features <- vector()
  selected.features.prev <- vector()
  best.features <- vector()
  best.imps <- vector()
  best.iter <- -1 ## initialize
  gain <- -1      ## initialize
  raw.gain.seq <- vector()
  curr.imp.prev <- curr.imp
  ##############################################

  for (iter in 1:iters.max) {

    g.matrix <- vector()

    for(g in 1:num.grad.avg) {

      delta <- ifelse(runif(p) >= 0.5, 1, -1) # random +1 or -1

      imp.plus <- curr.imp + perturb.amount*delta
      imp.plus <- pmax(imp.plus, imp.min)
      imp.plus <- pmin(imp.plus, imp.max)

      imp.minus <- curr.imp - perturb.amount*delta
      imp.minus <- pmax(imp.minus, imp.min)
      imp.minus <- pmin(imp.minus, imp.max)

      yplus  <- fs.perf(rdesc.grad.avg, imp.plus)[[1]]

      yminus <- fs.perf(rdesc.grad.avg, imp.minus)[[1]]

      g.matrix <- rbind(g.matrix, (yplus - yminus) / (2*perturb.amount*delta))
    }

    ghat.prev <- ghat
    ghat <- colMeans(g.matrix)

    ### if ghat is zero, set it to the previous ghat so that the search coninues
    if(identical(ghat, rep(0, p))) {
      ghat <- ghat.prev
    }

    if (iter == 1) {
      gain <- gain.min  ## initialization for iter == 1
      raw.gain.seq[iter] <- gain
    } else {
      imp.diff  <- curr.imp - curr.imp.prev
      ghat.diff <- ghat - ghat.prev
      gain <- sum(imp.diff*imp.diff) / abs(sum(imp.diff*ghat.diff))
      gain <- ifelse(is.nan(gain) | is.infinite(gain), gain.min, gain)
      gain <- max(gain.min, min(gain.max, gain)) ## limit between gain.min and gain.max
      raw.gain.seq[iter] <- gain
      if (iter >= 1 + num.gain.smoothing) {
        gain <- mean(raw.gain.seq[(iter - num.gain.smoothing + 1):iter]) ## gain smoothing
      }
    }

    curr.imp.prev <- curr.imp
    curr.imp <- curr.imp - gain*ghat
    curr.imp <- pmin(curr.imp, imp.max)
    curr.imp <- pmax(curr.imp, imp.min)

    selected.features.prev <- select.features(curr.imp.prev)

    selected.features <- select.features(curr.imp)

    # move until we get a new feature set,
    # but this time take small steps
    same.features.counter <- 1
    curr.imp.orig <- curr.imp
    while (identical(selected.features.prev, selected.features)) {
      curr.imp <- curr.imp.orig - same.features.counter * gain.min * ghat
      curr.imp <- pmin(curr.imp, imp.max)
      curr.imp <- pmax(curr.imp, imp.min)
      selected.features <- select.features(curr.imp)
      if (same.features.counter >= same.features.counter.max) break
      same.features.counter <- same.features.counter + 1
    } # end while

    # at this point, we (hopefully) have a new feature set:
    fs.perf.output <- fs.perf(rdesc.feat.eval, curr.imp)

    iter.results[['values']][iter]        <- round(opt.sign * fs.perf.output[[1]], 5) # mean
    iter.results[['stds']][iter]          <- round(fs.perf.output[[2]], 5) # sd
    iter.results[['gain.sequence']][iter] <- round(gain, 5)
    iter.results[['importances']][[iter]] <- round(curr.imp, 5)
    iter.results[['features']][[iter]]    <- getTaskFeatureNames(task)[select.features(curr.imp)]

    if (((opt.sign == 1) &  (iter.results[['values']][iter] <= best.value - stall.tolerance)) |
        ((opt.sign == -1) & (iter.results[['values']][iter] >= best.value + stall.tolerance))) {
      stall.counter <- 1
      best.iter  <- iter
      best.value <- iter.results[['values']][iter]
      best.std   <- iter.results[['stds']][iter]
      best.features <- selected.features
      best.imps <- curr.imp[best.features]
    } else {
      stall.counter <- stall.counter + 1
    }

    if (show.info) {
      if ((iter %% print.freq) == 0) {
        cat(format(as.character(iter), justify = 'left', width = 5),
            format(as.character(iter.results[['values']][iter]), justify = 'left', width = 7, scientific = FALSE),
            format(as.character(iter.results[['stds']][iter]),   justify = 'left', width = 7, scientific = FALSE),
            format(as.character(length(selected.features)), width = 7, justify = 'left'),
            best.value)
        if (stall.counter == 1) { cat(" *") }
        cat("\n")
      }
    }

    if (stall.counter > stall.limit) break

  } # end for iter

  ###################################################################################

  ## convert best feature indices to names
  best.features.names <- getTaskFeatureNames(task)[best.features]

  ## sort best features w.r.to decreasing imps
  best.features.names  <-  best.features.names[order(best.imps, decreasing = TRUE)]

  ## sort importance, which are regular spsa weights:
  best.features.importance <- round(sort(best.imps, decreasing = TRUE), 5)

  ## in this task, features order will be the same as in the original task
  task.spfs <- mlr::dropFeatures(task, features = getTaskFeatureNames(task)[-best.features]) ## this cannot be replaced with featureNames

  best.model <- mlr::train(learner = wrapper, task = task.spfs)

  run.time <- tictoc::toc(quiet = TRUE)
  run.time <- round(as.numeric(run.time$toc - run.time$tic)/60, 2)

  if (show.info) {
    message('\nBest iteration = ', best.iter)
    message('Number of selected features = ', length(best.features))
    message('Best measure value = ', iter.results[['values']][best.iter])
    message('Std. dev. of best measure = ', iter.results[['stds']][best.iter])
    message('Run time = ', run.time, ' minutes.')
  }

  return(list(
    wrapper = wrapper,
    measure = measure,
    task.spfs = task.spfs,
    best.model = best.model,
    iter.results = iter.results,
    features = best.features.names,
    importance = best.features.importance,
    num.features = length(best.features.names),
    total.iters = iter,
    best.iter = best.iter,
    best.value = iter.results[['values']][best.iter],
    best.std = iter.results[['stds']][best.iter],
    run.time = run.time,
    rdesc.feat.eval = rdesc.feat.eval
  ))
}
