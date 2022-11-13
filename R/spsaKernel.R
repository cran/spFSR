spsaKernel <- function(
  task,
  wrapper=NULL,
  scoring=NULL,
  #####

  perturb.amount = 0.05,
  #####

  gain.min = 0.01,
  gain.max = 2.0,
  #####

  change.min = 0.0,
  change.max = 0.2,
  #####

  bb.bottom.threshold = 10^(-8),
  #####

  mon.gain.A = 100,
  mon.gain.a = 0.75,
  mon.gain.alpha = 0.6,
  ####

  hot.start.num.ft.factor = 15L,
  hot.start.max.auto.num.ft = 75L,
  #####

  use.hot.start = TRUE,
  hot.start.range = 0.2,
  rf.n.estimators=50,
  gain.type = 'bb',
  num.features.selected = 0L,
  iters.max = 100L,
  stall.limit = 35L,
  n.samples.max = 2500L,
  ft.weighting = FALSE,
  encoding.type = "encode",
  is.debug = FALSE,
  random.state = 1,
  stall.tolerance = 10^(-8),
  rounding = 3,
  run.parallel = TRUE,
  n.jobs = NULL,
  show.info = TRUE,
  print.freq = 10L,
  #####

  num.cv.folds = 5L,
  num.cv.reps.eval = 3L,
  num.cv.reps.grad  = 1L,
  num.grad.avg = 4L,
  #####

  perf.eval.method = 'cv'

) {

  # Note: sections and comments will be deleted
  # 0. ON EXIT ----
  # Close the multisession workers by switching plan and stop parallel
  # regardless if the spfs results in error or not.

  on.exit(
    {
      future::plan(sequential)
    }
  )


  # 1. Check the versions of R and imported packages (PASSED) ----
  # Note: no need to check these which can be done in DESCRIPTION FILE.

  # 2. ARGUMENT CHECKS ----
  # make sure task, wrapper, and measure are defined properly

  if( !inherits(scoring, 'Measure') && !is.null(scoring)){
    stop('Not a supported performance measure in scoring. See mlr3 package for more details.')
  }else{


    if(  !any( scoring$id %in%  as.data.table(mlr_measures)[as.data.table(mlr_measures)$task_type == task$task_type,]$key )  && !is.null(scoring) ){
      stop( paste(scoring$id, 'measure is not supported by', task$task_type ,'task'))
    }
  }

  if (!is.element("Learner", class(wrapper)) && !is.null(wrapper)){
    stop('No Learner object in Wrapper. See mlr3 package for more details')
  }

  if (!(task$task_type == "classif" || task$task_type == "regr")){
    stop("A classification or regression task is required. See mlr3 package for more details")
  }

  stopifnot(!is.null(perf.eval.method), perf.eval.method %in% c('cv', 'resub'))

  stopifnot(is.logical(run.parallel))
  stopifnot(is.logical(show.info))

  if (run.parallel & !is.null(n.jobs)) {
    stopifnot(is.numeric(n.jobs),
              n.jobs %% 1 == 0,
              n.jobs >= 1)
  }

  if(run.parallel) {
    n.jobs.max <- parallel::detectCores()
    if(is.null(n.jobs)) {
      n.jobs <- n.jobs.max - 1
    } else {
      if(n.jobs > n.jobs.max){
        warning(paste0('Warning: Max number of cores is reset to the number of available cores = ', n.jobs.max, '.'))
        n.jobs <- n.jobs.max
      }
    }
    future::plan(multisession, workers = n.jobs)
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
		cat('\nPerturbation amount other than 0.05 is not recommended.\n')
	}

  stopifnot(is.numeric(gain.max),
            gain.max >= 0.001)

  stopifnot(is.numeric(gain.min),
            gain.min >= 0.001,
            gain.min <= gain.max)

  stopifnot(is.numeric(change.max),
            change.max >= 0.00)

  stopifnot(is.numeric(change.min),
            change.min >= 0.00,
            change.min <= gain.max)

  stopifnot(is.numeric(num.cv.folds),
            num.cv.folds %% 1 == 0,
            num.cv.folds >= 3)

  if (show.info) {
    stopifnot(is.numeric(print.freq),
              print.freq %% 1 == 0,
              print.freq >= 1)
  }

  stopifnot(is.numeric(hot.start.num.ft.factor),
            hot.start.num.ft.factor %% 1 == 0,
            hot.start.num.ft.factor >= 1)

  stopifnot(is.numeric(hot.start.max.auto.num.ft),
            hot.start.max.auto.num.ft %% 1 == 0,
            hot.start.max.auto.num.ft >= 1)

  stopifnot(is.logical(use.hot.start))

  stopifnot(is.numeric(hot.start.range),
            hot.start.range >=0)

  stopifnot(is.numeric(rf.n.estimators),
            rf.n.estimators %% 1 == 0,
            rf.n.estimators >=0)

  stopifnot(is.numeric(num.cv.reps.grad),
            num.cv.reps.grad %% 1 == 0,
            num.cv.reps.grad >= 1)

  stopifnot(is.numeric(num.cv.reps.eval),
            num.cv.reps.eval %% 1 == 0,
            num.cv.reps.eval >= 1)

  n.observations <- task$nrow # extract the number of observations

  p.all <- task$ncol -1 # extract the number of descriptive features

  stopifnot(is.numeric(num.features.selected),
            num.features.selected %% 1 == 0,
            num.features.selected >= 0,
            num.features.selected <= p.all)



  if (ft.weighting){
    if (num.features.selected >0 && use.hot.start && hot.start.range){
      stop('In case of feature weighting and hot start, either num.features.selected or hot.start.range must be 0')
    }
  }

  stopifnot(is.numeric(random.state),
            random.state %% 1 == 0)

  lgr::get_logger("mlr3")$set_threshold("warn")


  # 3. NORMALIZATION, RESAMPLING, and MLR OPTION CONFIGURATION ----
  set.seed(random.state)

  changeData = function(task, data_new){

    if (task$task_type =="classif"){
      task2 = as_task_classif(id=task$id, x=data_new, target=task$target_names, label=task$label, extra_args=task$extra_args, weights = task$weights)

      # if binary classification carry over positive
      if (length(task$class_names) ==2){
        task2$positive = task$positive
      }
    }
    else if (task$task_type == "regr"){
      task2 = as_task_regr(id=task$id, x=data_new, target=task$target_names, label=task$label, extra_args=task$extra_args, weights = task$weights)
    }

    if (!is.null(task$strata)){
      task2$add_strata(gsub("..stratum_", "", task$col_roles$stratum))
    }
    task2$man = task$man

    return(task2)
  }


  if (ft.weighting && (is.element('factor', task$feature_types$type) ||  is.element('ordered', task$feature_types$type) )){
    poe = po(encoding.type)

    warning(paste0('Warning: ft.weighting will convert factor and ordered feature types, ', poe$label, ' will be applied.') )

    task = changeData(task, poe$train(list(task))[[1]]$data())
    p.all <- task$ncol -1
  }





  shuffle.sample.data <- task$data()[sample(n.observations, min(n.observations, n.samples.max)),]
  task <- changeData(task=task, shuffle.sample.data)
  n.sample <- task$nrow # extract the new number of observations

  # wrapper left empty
  if (task$task_type == "classif"){
    if (is.null(wrapper)){
      wrapper = lrn("classif.ranger")
      wrapper$param_set$values = list(num.trees = rf.n.estimators)
    }
    if (is.null(scoring)){
      scoring = msr("classif.acc")
    }
  }
  else if (task$task_type == "regr"){
    if (is.null(wrapper)){
      wrapper = lrn("regr.ranger")
      wrapper$param_set$values = list(num.trees = rf.n.estimators)
    }
    if (is.null(scoring)){
      scoring = msr("regr.rsq")
    }
  }


  if (num.cv.reps.grad == 1) {
    rdesc.grad.avg <- rsmp("cv", folds = num.cv.folds)
    rdesc.grad.avg$instantiate(task)

  } else {
    rdesc.grad.avg <- rsmp("repeated_cv", folds = num.cv.folds,
                                            repeats = num.cv.reps.grad)
    rdesc.grad.avg$instantiate(task)

  }

  if (num.cv.reps.eval == 1) {
    rdesc.feat.eval <- rsmp("cv", folds = num.cv.folds)
    rdesc.feat.eval$instantiate(task)


  } else {
    rdesc.feat.eval <- rsmp("repeated_cv", folds = num.cv.folds,
                                             repeats = num.cv.reps.eval)
    rdesc.feat.eval$instantiate(task)

  }



  # prep algorithm
  ##############################
  p.active <- p.all
  idx.active <- 1:p.all
  imp.algo.start = rep(0.0, p.all)

  if (use.hot.start){
    if (task$task_type == 'classif'){
      hot.start.model <- lrn("classif.ranger")
    }
    else{
      hot.start.model <- lrn("regr.ranger")
    }
    hot.start.model$param_set$values = list(num.trees = rf.n.estimators, importance = "impurity")


    hot.start.model.fit <- hot.start.model$train(task)

    if (num.features.selected == 0){
      p.active <- min(p.all, hot.start.max.auto.num.ft)
    }
    else{
      p.active <- min(p.all, num.features.selected * hot.start.num.ft.factor)
    }

    hot.ft.imp <- hot.start.model.fit$importance()
    idx.hot.start.selected <- names(hot.ft.imp)[1:p.active]

    hot.ft.imp.selected <- hot.ft.imp[idx.hot.start.selected]
    idx.active <- idx.active[idx.hot.start.selected]

    x = task$data()
    x <- subset(x, select = idx.hot.start.selected)
    x <- cbind(x, subset(task$data(), select = task$target_names))

    task <- changeData(task = task, x)
    names(hot.ft.imp.selected) = NULL


    if (hot.start.range >0.0){
      hot.range = range(hot.ft.imp.selected)
      imp.algo.start = (hot.ft.imp.selected - hot.range[1])/(hot.range[2]-hot.range[1])*hot.start.range - (0.5*hot.start.range)
    }
    else{
      imp.algo.start = rep(0.0, p.active)
    }

    if (is.debug){
      print(paste0("Starting importance range: (", range(imp.algo.start)[1], ",", range(imp.algo.start)[2], ")"))
    }

  }


	# 4. SELECT FEATURES FUNCTION ----
  # given the imp. vector,
  # this function determines which features to select (as indices)
  get.selected.features <- function(imp) {
    if (num.features.selected == 0) {
      num.features.to.select <- sum(imp >= 0.0)
      if (num.features.to.select == 0) {
        num.features.to.select <- 1 ## select at least one!
      }
    } else {
      num.features.to.select <- min(length(imp), num.features.selected)
    }
    order(imp, decreasing = TRUE)[1:num.features.to.select]
  }

  ################################
  # this function performs either repeated f-fold CV or computes the resubstitution performance
  eval.feature.set <- function(rdesc, curr.imp) {
    selected.features <- get.selected.features(curr.imp)

    if (ft.weighting){
      selected.ft.imp = curr.imp[selected.features]
      num.neg.imp = length(selected.ft.imp[selected.ft.imp < 0])
      if (num.neg.imp > 0){
        stop(cat("Error in feature weighting: ",num.neg.imp," negative weights encountered
                            - try reducing number of selected features or set it to 0 for auto."))
      }

      feature_values = subset(task$data(), select = task$col_roles$feature)

      # apply feature weight
      weighted_feature_values <- as.data.frame(t(t(feature_values)*curr.imp))

      weighted_feature_values <- cbind(weighted_feature_values, subset(task$data(), select = task$target_names))


      # update the tasks with the new weights
      task <- changeData(task = task, weighted_feature_values)
    }

    task.fs <- changeData(task=task, data = cbind(subset(task$data(), select = setdiff(colnames(task$data()), task$target_names)[selected.features]), subset(task$data(), select = task$target_names)))

    if (perf.eval.method == "cv") {



      resample_result <- resample(learner = wrapper, task = task.fs,
                              resampling = rdesc)

      result = resample_result$score(scoring)[,9]



      best.value.mean <- mean(result)
      best.value.std  <- sd(result)
    }
    else {
      stop(paste0("Unsupported performance evaluation method: ", perf.eval.method))
    }

    list(opt.sign * best.value.mean, best.value.std)
  } # end eval.feature.set

  # we need the opt. sign to correctly multiply the best performance values
  opt.sign <- 1 # +1 for minimization, -1 for maximization
  if (scoring$minimize == FALSE) {opt.sign <- -1}

  clip.change <- function(raw.change){
    # make sure change in the importance vector is bounded
    change.sign = raw.change > 0.0
    change.abs.clipped = (abs(raw.change) - pmin(abs(raw.change), change.max)) + (abs(raw.change) - pmax(abs(raw.change), change.min))
    change.clipped = (2*change.sign-1) * change.abs.clipped
    return (-1*(change.clipped-raw.change))
  }




  init.parameters <- function(){

    if (!is.null(imp.algo.start)){
      curr.imp = imp.algo.start
      if (show.info){
        if (is.debug){
          print(paste0("Starting importance range: (",min(curr.imp),",",max(curr.imp),")"))
        }
      }
    }
    else{
      curr.imp = rep(0.0, p.active)  # initialize starting importance to (0,...,0)
    }

    ghat = rep(0.0, p.active)
    curr.imp.prev = curr.imp

    return (list(curr.imp, ghat, curr.imp.prev))
  }





  ##############################################
  ### initializations:
  stall.counter <- 1
  best.value <- opt.sign * Inf
  best.std <- Inf
  ##############################################

  holder = init.parameters()
  curr.imp = holder[[1]]
  ghat = holder[[2]]
  curr.imp.prev = holder[[3]]

  if (show.info ) {
    cat('\n\nspfs begins:\n')
    cat('\nWrapper:' , wrapper$id)
    cat('\nHot start:', use.hot.start)
    if (use.hot.start){
      cat('\nHot start range:', hot.start.range)
    }
    cat('\nFeature Weighting:', ft.weighting)
    cat('\nScoring: ' , scoring$id)
    cat('\nNumber of Jobs:', n.jobs)
    cat('\nNumber of observations in the dataset:', n.observations)
    cat('\nNumber of observations used:', n.sample)
    cat('\nNumber of features avaliable:', p.all)
    cat('\nNumber of selected features:',num.features.selected, '\n')
    cat('\niter ', 'value', '  st.dev', ' num.ft ', 'best.value\n')
  }

  # iter.results stores results per spfs iteration
  iter.results <- list(values = numeric(0),
                       st.devs = numeric(0),
                       gains = numeric(0),
                       importance = list(),
                       feature.indices = list())

  ##############################################
  ### initializations:
  selected.features <- vector()
  selected.features.prev <- vector()
  best.features <- vector()
  best.imps <- vector()
  best.iter <- -1 ## initialize
  gain <- -1      ## initialize
  raw.gain.seq <- vector()
  ##############################################
  run.time <- -1
  tictoc::tic()

  iter = 0
  while (iter < iters.max){
    iter = iter + 1
    g.matrix <- vector()

    curr.imp.sel.ft.sorted <- sort(get.selected.features(curr.imp))

    for(g in 1:num.grad.avg) {

      bad.perturb.counter = 0
      while (bad.perturb.counter < stall.limit){

        delta <- ifelse(runif(p.active) >= 0.5, 1, -1) # random +1 or -1

        imp.plus <- curr.imp + perturb.amount*delta
        imp.minus <- curr.imp - perturb.amount*delta


        imp.plus.sel.ft.sorted = sort(get.selected.features(imp.plus))
        imp.minus.sel.ft.sorted = sort(get.selected.features(imp.minus))


        if ((!identical(curr.imp.sel.ft.sorted, imp.plus.sel.ft.sorted)) & (!identical(curr.imp.sel.ft.sorted, imp.minus.sel.ft.sorted))){
          break
        }
        else{
          bad.perturb.counter = bad.perturb.counter + 1
        }
      }

      if (is.debug){
        if (bad.perturb.counter > 0){
          print(paste0("=> iter_no: ",iter, ", bad_perturb_counter: ",bad.perturb.counter, " at gradient iteration ",g))
        }
      }
      yplus  <- eval.feature.set(rdesc.grad.avg, imp.plus)[[1]]
      yminus <- eval.feature.set(rdesc.grad.avg, imp.minus)[[1]]

      if (!identical(yplus, yminus)){
        g.curr <- (yplus - yminus) / (2*perturb.amount*delta)
        g.matrix <- rbind(g.matrix, g.curr)
      }
      else{
        # unfortunately there will be one less gradient in the gradient averaging
        if (is.debug){
          print(paste0("=> iter_no: ",iter,", y_plus == y_minus at gradient iteration ",g))

        }
      }
    }
    if (is.debug){
      if (length(g.matrix) < num.grad.avg){
        print(paste0("=> iter_no: ",iter,", zero gradient(s) encountered: only ",nrow(g.matrix)," gradients averaged."))
      }
    }

    ghat.prev <- ghat
    #ghat <- colMeans(g.matrix)

    ### if ghat is zero, set it to the previous ghat so that the search continues
    if(length(g.matrix) == 0) {
      if (is.debug){
        print(paste0('=> iter_no: ", iter, ", no proper gradient found, searching in the previous direction.'))
      }

      ghat <- ghat.prev
    }
    else{
      g.matrix.avg <- colMeans(g.matrix)

      if (identical(g.matrix.avg, rep(0, p.active))){
        if (is.debug){
          print(paste0("=> iter_no: ",iter,", zero gradient encountered, searching in the previous direction."))
        }
        ghat <- ghat.prev

      }
      else{
        ghat <- g.matrix.avg
      }
    }

    if (gain.type == 'bb'){
      if (iter == 1) {
        gain <- gain.min  ## initialization for iter == 1
        raw.gain.seq[iter] <- gain
      } else {
        imp.diff  <- curr.imp - curr.imp.prev
        ghat.diff <- ghat - ghat.prev
        bb.bottom <-  sum(imp.diff*ghat.diff) # -1 due to maximization in SPSA
        # make sure we don't end up with division by zero or negative gains:
        if (bb.bottom < bb.bottom.threshold ){
          gain <- gain.min
        }
        else{
          gain <- sum(imp.diff*imp.diff) / bb.bottom
        }

      }
    }
    else if (gain.type == 'mon'){
      gain = mon.gain.a / ((iter + mon.gain.A) ** mon.gain.alpha)
    }
    else{
      stop('Error: unknown gain type')
    }


    # gain bounding
    gain <- max(gain.min, min(gain.max, gain)) ## limit between gain.min and gain.max

    if (is.debug){
      print(paste0("=> iter_no: ",iter, ", iteration gain = ",round(gain, rounding)))
    }

      curr.imp.prev <- curr.imp
      # make sure change is not too much
      curr.change.raw = gain * ghat
      curr.change.clipped = clip.change(curr.change.raw)
      # we use "+" below so that SPSA maximizes
      curr.imp = curr.imp - curr.change.clipped

      selected.features.prev <- get.selected.features(curr.imp.prev)
      selected.features <- get.selected.features(curr.imp)
      selected.features.prev.sorted = sort(selected.features.prev)


      # move until we get a new feature set,
      # but this time take small steps
      same.feature.counter <- 0
      curr.imp.orig <- curr.imp
      same.feature.step.size = (gain.max - gain.min) / stall.limit

      while (identical(selected.features.prev, sort(selected.features))) {
        same.feature.counter <- same.feature.counter + 1
        curr.step.size = (gain.min + same.feature.counter * same.feature.step.size)

        curr.change.raw = curr.step.size * ghat
        curr.change.clipped = clip.change(curr.change.raw)
        curr.imp = curr.imp.orig - curr.change.clipped
        selected.features = get.selected.features(curr.imp)
        if (same.feature.counter >= stall.limit){
          break
        }
      }

      if (is.debug){
        if (same.feature.counter > 0){
          print(paste0("=> iter_no: ",iter,", same.feature.counter = ",same.feature.counter))
        }
      }

      # at this point, we (hopefully) have a new feature set:
      eval.feature.set.output <- eval.feature.set(rdesc.feat.eval, curr.imp)

      iter.results[['values']][iter]        <- round(opt.sign*eval.feature.set.output[[1]], rounding) # mean
      iter.results[['st.devs']][iter]          <- round(eval.feature.set.output[[2]], rounding) # sd
      iter.results[['gains']][iter] <- round(gain, rounding)
      iter.results[['importance']][[iter]] <- round(curr.imp, rounding)
      iter.results[['feature.indices']][[iter]]    <- get.selected.features(curr.imp)

      if ((((opt.sign == -1) & (iter.results[['values']][iter] >= (best.value + stall.tolerance)))) ||
          ((opt.sign == 1) & (iter.results[['values']][iter] <= (best.value - stall.tolerance)))){
        stall.counter <- 1
        best.iter  <- iter
        best.value <- iter.results[['values']][iter]
        best.std   <- iter.results[['st.devs']][iter]
        best.features <- selected.features
        best.imps <- curr.imp[best.features]
      } else {
        stall.counter <- stall.counter + 1
      }

      if (show.info) {
        if (((iter %% print.freq) == 0)||(iter==1)) {
          cat(format(as.character(iter), justify = 'left', width = 5),
              format(as.character(iter.results[['values']][iter]), justify = 'left', width = 7, scientific = FALSE),
              format(as.character(iter.results[['st.devs']][iter]),   justify = 'left', width = 7, scientific = FALSE),
              format(as.character(length(selected.features)), width = 7, justify = 'left'),
              best.value)
          if (stall.counter == 1) { cat(" *") }
          cat("\n")
        }
      }

      if (same.feature.counter >= stall.limit){
        # search stalled, start from scratch!
        if (show.info){
          print(paste0("===> iter_no: ",iter, " same feature stall limit reached, initializing search..."))
        }

        stall.counter = 1  # reset the stall counter
        holder = init.parameters()
        curr.imp = holder[[1]]
        ghat = holder[[2]]
        curr.imp.prev = holder[[3]]

      }

      if (stall.counter >= stall.limit){
        # search stalled, start from scratch!
        if (show.info){
          print(paste0("===> iter_no: ",iter, " iteration stall limit reached, initializing search..."))
        }
        stall.counter = 1  # reset the stall counter to give this solution enough time
        holder = init.parameters()  # set _curr_imp and _g_hat to vectors of zeros
        curr.imp = holder[[1]]
        ghat = holder[[2]]
        curr.imp.prev = holder[[3]]
      }

  } # end while iter

  ###################################################################################

  ## convert best feature indices to names
  best.features.names <- setdiff(colnames(task$data()), task$target_names)[best.features]

  ## sort best features w.r.to decreasing imps
  best.features.names  <-  best.features.names[order(best.imps, decreasing = TRUE)]

  ## sort importance, which are regular spsa weights:
  best.features.importance <- round(sort(best.imps, decreasing = TRUE), 5)

  ## in this task, features order will be the same as in the original task
  task.spfs <- changeData(task=task, cbind(subset(task$data(), select = setdiff(colnames(task$data()), task$target_names)[best.features]), subset(task$data(), select = task$target_names)))

  best.model <- wrapper$train(task = task.spfs)


  result.df = data.frame( t(task$col_roles$feature %in% best.features.names))

  colnames(result.df) <- task$col_roles$feature

  result.df$features = paste(best.features.names, collapse = ", ")
  result.df$new = best.value

  names(result.df)[length(result.df)] = scoring$id


  run.time <- tictoc::toc(quiet = TRUE)
  run.time <- round(as.numeric(run.time$toc - run.time$tic)/60, 2)

  if (show.info) {

    cat('\nspFSR completed in', run.time, 'minutes')
    cat('\nBest Value =', iter.results[['values']][best.iter], "with", length(best.features), "features on iteration", best.iter)
    cat('\n\n')
  }

  lgr::get_logger("mlr3")$set_threshold("info")

  return(list(
    wrapper = wrapper,
    scoring = scoring,
    task.spfs = task.spfs,
    best.model = best.model,
    iter.results = iter.results,
    features = best.features.names,
    importance = best.features.importance,
    num.features = length(best.features.names),
    total.iters = iter,
    best.iter = best.iter,
    best.value = iter.results[['values']][best.iter],
    best.std = iter.results[['st.devs']][best.iter],
    run.time = run.time,
    results = result.df
  ))
}

